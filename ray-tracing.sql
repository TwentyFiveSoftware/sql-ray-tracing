-- MySQL:
-- SET SESSION cte_max_recursion_depth = 1000000;
-- SET SESSION group_concat_max_len = 1000000000000;

-- render
WITH
    RECURSIVE
    settings AS (
        SELECT 300   AS width,
               200   AS height,
               0     AS camera_origin_x,
               0     AS camera_origin_y,
               0     AS camera_origin_z,
               2.0   AS viewport_height,
               1.0   AS focal_length,
               0.001 AS sphere_collision_t_min
    ),
    spheres (sphere_id, center_x, center_y, center_z, radius, material_type, albedo_r, albedo_g, albedo_b) AS (
        SELECT 0, 0, 0, 1, 0.5, 0, 0.9, 0.9, 0.9
    ),
    derived_constants AS (
        SELECT CAST(width AS FLOAT) / height * viewport_height AS viewport_width
        FROM settings
    ),
    pixel_ids (pixel_id) AS (
        SELECT 0
        UNION ALL
        SELECT pixel_id + 1
        FROM pixel_ids
        WHERE pixel_id + 1 < (
            SELECT width * height
            FROM settings
        )
    ),
    pixel_coordinates (pixel_id, x, y, u, v) AS (
        SELECT pixel_id,
               x,
               y,
               CAST(x AS FLOAT) / (width - 1),
               CAST(y AS FLOAT) / (height - 1)
        FROM (
            SELECT pixel_id,
                   MOD(pixel_id, width)    AS x,
                   FLOOR(pixel_id / width) AS y,
                   width,
                   height
            FROM pixel_ids,
                 (
                     SELECT width, height
                     FROM settings
                 ) AS dimensions
        ) AS pixel_xy
    ),
    rays (ray_id, pixel_id, ray_origin_x, ray_origin_y, ray_origin_z, ray_direction_x, ray_direction_y,
          ray_direction_z) AS (
        SELECT ray_id,
               pixel_id,
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
            SELECT y * width + x                                  AS ray_id,
                   pixel_id,
                   camera_origin_x                                AS ray_origin_x,
                   camera_origin_y                                AS ray_origin_y,
                   camera_origin_z                                AS ray_origin_z,
                   -camera_origin_x + (u - 0.5) * viewport_width  AS ray_direction_x,
                   -camera_origin_y + (v - 0.5) * viewport_height AS ray_direction_y,
                   focal_length - camera_origin_z                 AS ray_direction_z
            FROM pixel_coordinates,
                 (
                     SELECT camera_origin_x, camera_origin_y, camera_origin_z, focal_length, viewport_height, width
                     FROM settings
                 ) AS s,
                 (
                     SELECT viewport_width
                     FROM derived_constants
                 ) AS c
        ) AS not_normalized_rays
    ),
    closest_ray_intersections (ray_id, sphere_id, t) AS (
        WITH
            sphere_intersection_calc (ray_id, sphere_id, a, half_b, c) AS (
                SELECT ray_id,
                       sphere_id,
                       ray_direction_x * ray_direction_x + ray_direction_y * ray_direction_y +
                       ray_direction_z * ray_direction_z,
                       (ray_origin_x - spheres.center_x) * ray_direction_x +
                       (ray_origin_y - spheres.center_y) * ray_direction_y +
                       (ray_origin_z - spheres.center_z) * ray_direction_z,
                       ((ray_origin_x - spheres.center_x) * (ray_origin_x - spheres.center_x) +
                        (ray_origin_y - spheres.center_y) * (ray_origin_y - spheres.center_y) +
                        (ray_origin_z - spheres.center_z) * (ray_origin_z - spheres.center_z)) -
                       spheres.radius * spheres.radius
                FROM rays,
                     spheres
            ),
            sphere_intersection_discriminants (ray_id, sphere_id, a, half_b, c, discriminant) AS (
                SELECT *, half_b * half_b - a * c
                FROM sphere_intersection_calc
            ),
            sphere_intersection_roots (ray_id, sphere_id, root_1, root_2) AS (
                SELECT ray_id,
                       sphere_id,
                       (-half_b - SQRT(discriminant)) / a,
                       (-half_b + SQRT(discriminant)) / a
                FROM sphere_intersection_discriminants
                WHERE discriminant >= 0
            ),
            sphere_intersection_t (ray_id, sphere_id, t) AS (
                SELECT ray_id,
                       sphere_id,
                       CASE
                           WHEN root_1 > sphere_collision_t_min AND root_1 <= root_2 THEN root_1
                           WHEN root_2 > sphere_collision_t_min AND root_2 < root_1 THEN root_2
                           ELSE -1.0
                           END
                FROM sphere_intersection_roots,
                     settings
            ),
            closest_sphere_intersection AS (
                SELECT sphere_intersection_t.ray_id,
                       sphere_id,
                       sphere_intersection_t.t
                FROM sphere_intersection_t
                JOIN (
                    SELECT ray_id, MIN(t) AS t
                    FROM sphere_intersection_t
                    WHERE t >= 0.0
                    GROUP BY ray_id
                ) lowest_t ON sphere_intersection_t.ray_id = lowest_t.ray_id AND sphere_intersection_t.t = lowest_t.t
            )
        SELECT *
        FROM closest_sphere_intersection
    ),
    hit_records (pixel_id, ray_direction_y, hit, normal_x, normal_y, normal_z) AS (
        WITH
            intersection_point AS (
                SELECT rays.*,
                       sphere_id,
                       t,
                       ray_origin_x + ray_direction_x * t AS point_x,
                       ray_origin_y + ray_direction_y * t AS point_y,
                       ray_origin_z + ray_direction_z * t AS point_z
                FROM closest_ray_intersections
                LEFT JOIN rays ON closest_ray_intersections.ray_id = rays.ray_id
            ),
            intersection_outward_normal AS (
                SELECT intersection_point.*,
                       (point_x - spheres.center_x) / spheres.radius AS normal_x,
                       (point_y - spheres.center_y) / spheres.radius AS normal_y,
                       (point_z - spheres.center_z) / spheres.radius AS normal_z
                FROM intersection_point
                LEFT JOIN spheres ON intersection_point.sphere_id = spheres.sphere_id
            ),
            intersection_is_front_face AS (
                SELECT *,
                       ray_direction_x * normal_x + ray_direction_y * normal_y + ray_direction_z * normal_z <
                       0.0 AS is_front_face
                FROM intersection_outward_normal
            ),
            intersections (ray_id, sphere_id, t, point_x, point_y, point_z, normal_x, normal_y, normal_z, is_front_face)
                AS (
                SELECT ray_id,
                       sphere_id,
                       t,
                       point_x,
                       point_y,
                       point_z,
                       CASE WHEN is_front_face THEN normal_x ELSE -normal_x END,
                       CASE WHEN is_front_face THEN normal_y ELSE -normal_y END,
                       CASE WHEN is_front_face THEN normal_z ELSE -normal_z END,
                       is_front_face
                FROM intersection_is_front_face
            )
        SELECT pixel_id,
               ray_direction_y,
               t IS NOT NULL,
               normal_x,
               normal_y,
               normal_z
        FROM rays
        LEFT OUTER JOIN intersections ON rays.ray_id = intersections.ray_id
    ),
    ray_colors (pixel_id, r, g, b) AS (
        SELECT pixel_id,
               CASE
                   WHEN hit THEN (normal_x + 1.0) * 0.5
                   ELSE
                       1.0 * (1.0 - ((ray_direction_y + 1.0) * 0.5)) + 0.3 * ((ray_direction_y + 1.0) * 0.5)
                   END,
               CASE
                   WHEN hit THEN (normal_y + 1.0) * 0.5
                   ELSE
                       1.0 * (1.0 - ((ray_direction_y + 1.0) * 0.5)) + 0.5 * ((ray_direction_y + 1.0) * 0.5)
                   END,
               CASE
                   WHEN hit THEN (normal_z + 1.0) * 0.5
                   ELSE
                       1.0 * (1.0 - ((ray_direction_y + 1.0) * 0.5)) + 0.8 * ((ray_direction_y + 1.0) * 0.5)
                   END
        FROM hit_records
    ),
    pixels (pixel_id, r, g, b) AS (
        SELECT pixel_id,
               FLOOR(r * 0xFF),
               FLOOR(g * 0xFF),
               FLOOR(b * 0xFF)
        FROM ray_colors
    ),
    image_pixel_rgb (rgb) AS (
        SELECT CONCAT(r, ' ', g, ' ', b)
        FROM pixels
        ORDER BY pixel_id
    ),
    image (image_in_ppm_format) AS (
--         SELECT CONCAT('P3', '\n', width, ' ', height, '\n', '255', '\n', pixels, '\n') -- MySQL
        SELECT CONCAT('P3', E'\n', width, ' ', height, E'\n', '255', E'\n', pixels, E'\n') -- Postgres
        FROM (
--                  SELECT GROUP_CONCAT(rgb SEPARATOR '\n') AS pixels -- MySQL
                 SELECT STRING_AGG(rgb, E'\n') AS pixels -- Postgres
                 FROM image_pixel_rgb
             ) AS p,
             (
                 SELECT width, height
                 FROM settings
             ) AS dimensions
    )
SELECT image_in_ppm_format
FROM image
