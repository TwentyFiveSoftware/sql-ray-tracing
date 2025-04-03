SET SESSION cte_max_recursion_depth = 1000000;
SET SESSION group_concat_max_len = 1000000000000;

SET @WIDTH = 300;
SET @HEIGHT = 200;

SET @CAMERA_ORIGIN_X = 0;
SET @CAMERA_ORIGIN_Y = 0;
SET @CAMERA_ORIGIN_Z = 0;

SET @VIEWPORT_HEIGHT = 2.0;
SET @FOCAL_LENGTH = 1.0;

SET @ASPECT_RATIO = CAST(@WIDTH AS FLOAT) / @HEIGHT;
SET @VIEWPORT_WIDTH = @ASPECT_RATIO * @VIEWPORT_HEIGHT;

SET @CAMERA_UPPER_LEFT_CORNER_X = @VIEWPORT_WIDTH * -0.5 - @CAMERA_ORIGIN_X;
SET @CAMERA_UPPER_LEFT_CORNER_Y = @VIEWPORT_HEIGHT * -0.5 - @CAMERA_ORIGIN_Y;
SET @CAMERA_UPPER_LEFT_CORNER_Z = @FOCAL_LENGTH - @CAMERA_ORIGIN_Z;

SET @SPHERE_COLLISION_T_MIN = 0.001;

# EXPLAIN ANALYZE
WITH
    RECURSIVE
    spheres (sphere_id, center_x, center_y, center_z, radius) AS (
        SELECT 0, 0, 0, 1, 0.5
    ),
    pixel_ids (pixel_id) AS (
        SELECT 0
        UNION ALL
        SELECT pixel_id + 1
        FROM pixel_ids
        WHERE pixel_id + 1 < @WIDTH * @HEIGHT
    ),
    pixel_coordinates (x, y) AS (
        SELECT pixel_id MOD @WIDTH, FLOOR(pixel_id / @WIDTH)
        FROM pixel_ids
    ),
    pixel_uv (x, y, u, v) AS (
        SELECT x,
               y,
               CAST(x AS FLOAT) / (@WIDTH - 1),
               CAST(y AS FLOAT) / (@HEIGHT - 1)
        FROM pixel_coordinates
    ),
    rays (ray_id, pixel_x, pixel_y, ray_origin_x, ray_origin_y, ray_origin_z, ray_direction_x, ray_direction_y,
          ray_direction_z) AS (
        SELECT ray_id,
               pixel_x,
               pixel_y,
               ray_origin_x,
               ray_origin_y,
               ray_origin_z,
               ray_direction_x / SQRT(ray_direction_x * ray_direction_x + ray_direction_y * ray_direction_y +
                                      ray_direction_z * ray_direction_z),
               ray_direction_y / SQRT(ray_direction_x * ray_direction_x + ray_direction_y * ray_direction_y +
                                      ray_direction_z * ray_direction_z),
               ray_direction_z / SQRT(ray_direction_x * ray_direction_x + ray_direction_y * ray_direction_y +
                                      ray_direction_z * ray_direction_z)
        FROM (
            SELECT y * @WIDTH + x                                     AS ray_id,
                   x                                                  AS pixel_x,
                   y                                                  AS pixel_y,
                   @CAMERA_ORIGIN_X                                   AS ray_origin_x,
                   @CAMERA_ORIGIN_Y                                   AS ray_origin_y,
                   @CAMERA_ORIGIN_Z                                   AS ray_origin_z,
                   @CAMERA_UPPER_LEFT_CORNER_X + @VIEWPORT_WIDTH * u  AS ray_direction_x,
                   @CAMERA_UPPER_LEFT_CORNER_Y + @VIEWPORT_HEIGHT * v AS ray_direction_y,
                   @CAMERA_UPPER_LEFT_CORNER_Z                        AS ray_direction_z
            FROM pixel_uv
        ) AS not_normalized_rays
    ),
    ray_sphere_collisions_discriminant (ray_id, sphere_id, a, half_b, c, discriminant) AS (
        SELECT *,
               half_b * half_b - a * c
        FROM (
            SELECT ray_id,
                   sphere_id,
                   ray_direction_x * ray_direction_x + ray_direction_y * ray_direction_y +
                   ray_direction_z * ray_direction_z                   AS a,
                   (ray_origin_x - spheres.center_x) * ray_direction_x +
                   (ray_origin_y - spheres.center_y) * ray_direction_y +
                   (ray_origin_z - spheres.center_z) * ray_direction_z AS half_b,
                   ((ray_origin_x - spheres.center_x) * (ray_origin_x - spheres.center_x) +
                    (ray_origin_y - spheres.center_y) * (ray_origin_y - spheres.center_y) +
                    (ray_origin_z - spheres.center_z) * (ray_origin_z - spheres.center_z)) -
                   spheres.radius * spheres.radius                     AS c
            FROM rays,
                 spheres
        ) AS ray_sphere_collisions_discriminant_calc
    ),
    ray_sphere_collisions_roots (ray_id, sphere_id, root_1, root_2) AS (
        SELECT ray_id,
               sphere_id,
               (-half_b - SQRT(discriminant)) / a,
               (-half_b + SQRT(discriminant)) / a
        FROM ray_sphere_collisions_discriminant
        WHERE discriminant >= 0
    ),
    ray_sphere_collisions_t (ray_id, sphere_id, t) AS (
        SELECT ray_id,
               sphere_id,
               CASE
                   WHEN root_1 > @SPHERE_COLLISION_T_MIN AND root_1 <= root_2 THEN root_1
                   WHEN root_2 > @SPHERE_COLLISION_T_MIN AND root_2 < root_1 THEN root_2
                   ELSE -1.0
                   END
        FROM ray_sphere_collisions_roots
    ),
    ray_collisions_closest (ray_id, sphere_id, t) AS (
        SELECT ray_sphere_collisions_t.ray_id,
               sphere_id,
               ray_sphere_collisions_t.t
        FROM ray_sphere_collisions_t
        JOIN (
            SELECT ray_id, MIN(t) AS t
            FROM ray_sphere_collisions_t
            WHERE t >= 0.0
            GROUP BY ray_id
        ) lowest_t ON ray_sphere_collisions_t.ray_id = lowest_t.ray_id AND ray_sphere_collisions_t.t = lowest_t.t
    ),
    ray_collisions (ray_id, sphere_id, t, point_x, point_y, point_z, normal_x, normal_y, normal_z, is_front_face)
        AS (
        SELECT ray_id,
               sphere_id,
               t,
               point_x,
               point_y,
               point_z,
               IF(is_front_face, normal_x, -normal_x),
               IF(is_front_face, normal_y, -normal_y),
               IF(is_front_face, normal_z, -normal_z),
               is_front_face
        FROM (
            SELECT *,
                   ray_direction_x * normal_x + ray_direction_y * normal_y + ray_direction_z * normal_z <
                   0.0 AS is_front_face
            FROM (
                SELECT ray_collisions_point.*,
                       (point_x - spheres.center_x) / spheres.radius AS normal_x,
                       (point_y - spheres.center_y) / spheres.radius AS normal_y,
                       (point_z - spheres.center_z) / spheres.radius AS normal_z
                FROM (
                    SELECT rays.*,
                           sphere_id,
                           t,
                           ray_origin_x + ray_direction_x * t AS point_x,
                           ray_origin_y + ray_direction_y * t AS point_y,
                           ray_origin_z + ray_direction_z * t AS point_z
                    FROM ray_collisions_closest
                    LEFT JOIN rays ON ray_collisions_closest.ray_id = rays.ray_id
                ) AS ray_collisions_point
                LEFT JOIN spheres ON ray_collisions_point.sphere_id = spheres.sphere_id
            ) AS ray_collisions_outward_normal
        ) AS ray_collisions_is_front_face
    ),
    ray_hit_records (pixel_x, pixel_y, ray_direction_y, hit, normal_x, normal_y, normal_z) AS (
        SELECT pixel_x,
               pixel_y,
               ray_direction_y,
               t IS NOT NULL,
               normal_x,
               normal_y,
               normal_z
        FROM rays
        LEFT OUTER JOIN ray_collisions ON rays.ray_id = ray_collisions.ray_id
    ),
    ray_colors (pixel_x, pixel_y, r, g, b) AS (
        SELECT pixel_x,
               pixel_y,
               IF(hit, (normal_x + 1.0) * 0.5,
                  1.0 * (1.0 - ((ray_direction_y + 1.0) * 0.5)) + 0.3 * ((ray_direction_y + 1.0) * 0.5)),
               IF(hit, (normal_y + 1.0) * 0.5,
                  1.0 * (1.0 - ((ray_direction_y + 1.0) * 0.5)) + 0.5 * ((ray_direction_y + 1.0) * 0.5)),
               IF(hit, (normal_z + 1.0) * 0.5,
                  1.0 * (1.0 - ((ray_direction_y + 1.0) * 0.5)) + 0.8 * ((ray_direction_y + 1.0) * 0.5))
        FROM ray_hit_records
    ),
    pixels (x, y, r, g, b) AS (
        SELECT pixel_x,
               pixel_y,
               FLOOR(r * 0xFF),
               FLOOR(g * 0xFF),
               FLOOR(b * 0xFF)
        FROM ray_colors
    ),
    image_pixel_rows (image_pixel_row) AS (
        SELECT CONCAT(r, ' ', g, ' ', b)
        FROM pixels
        ORDER BY y, x
    ),
    image_pixels (image_pixels) AS (
        SELECT GROUP_CONCAT(image_pixel_row SEPARATOR '\n')
        FROM image_pixel_rows
    )
SELECT CONCAT('P3\n', @WIDTH, ' ', @HEIGHT, '\n255\n', image_pixels)
# INTO OUTFILE '/var/lib/mysql-files/render.ppm' FIELDS TERMINATED BY '' ESCAPED BY '' LINES TERMINATED BY '\n'
FROM image_pixels;
