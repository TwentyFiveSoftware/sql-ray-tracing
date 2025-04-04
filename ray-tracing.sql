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
               0.001 AS sphere_collision_t_min,
               1     AS max_depth
    ),
    spheres (sphere_id, sphere_center_x, sphere_center_y, sphere_center_z, sphere_radius, sphere_material_type,
             sphere_albedo_r, sphere_albedo_g, sphere_albedo_b) AS (
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
    rays (pixel_id, ray_id, ray_depth, should_trace, ray_origin_x, ray_origin_y, ray_origin_z, ray_direction_x,
          ray_direction_y, ray_direction_z, ray_color_r, ray_color_g, ray_color_b) AS (
        -- initial rays (origin at the camera):
        WITH
            non_normalized_rays AS (
                SELECT pixel_id,
                       pixel_id * (max_depth + 1)                     AS ray_id,
                       0                                              AS ray_depth,
                       TRUE                                           AS should_trace,
                       camera_origin_x                                AS ray_origin_x,
                       camera_origin_y                                AS ray_origin_y,
                       camera_origin_z                                AS ray_origin_z,
                       -camera_origin_x + (u - 0.5) * viewport_width  AS ray_direction_x,
                       -camera_origin_y + (v - 0.5) * viewport_height AS ray_direction_y,
                       focal_length - camera_origin_z                 AS ray_direction_z
                FROM pixel_coordinates,
                     settings,
                     derived_constants
            ),
            rays_with_direction_length AS (
                SELECT *,
                       SQRT(ray_direction_x * ray_direction_x + ray_direction_y * ray_direction_y +
                            ray_direction_z * ray_direction_z) AS direction_length
                FROM non_normalized_rays
            ),
            normalized_rays AS (
                SELECT pixel_id,
                       ray_id,
                       ray_depth,
                       should_trace,
                       ray_origin_x,
                       ray_origin_y,
                       ray_origin_z,
                       ray_direction_x / direction_length AS ray_direction_x,
                       ray_direction_y / direction_length AS ray_direction_y,
                       ray_direction_z / direction_length AS ray_direction_z
                FROM rays_with_direction_length
            ),
            rays_with_background_color AS (
                SELECT *,
                       1.0 * (1.0 - ((ray_direction_y + 1.0) * 0.5))
                           + 0.3 * ((ray_direction_y + 1.0) * 0.5) AS ray_color_r,
                       1.0 * (1.0 - ((ray_direction_y + 1.0) * 0.5))
                           + 0.5 * ((ray_direction_y + 1.0) * 0.5) AS ray_color_g,
                       1.0 * (1.0 - ((ray_direction_y + 1.0) * 0.5))
                           + 0.8 * ((ray_direction_y + 1.0) * 0.5) AS ray_color_b
                FROM normalized_rays
            )
        SELECT *
        FROM rays_with_background_color

        UNION ALL

        -- recursive ray intersection and color calculation:
        SELECT *
        FROM (
            WITH
                last_ray_per_pixel AS (
                    WITH
                        all_rays AS (
                            SELECT *
                            FROM rays
                        )
                    SELECT all_rays.*
                    FROM (
                        SELECT pixel_id, MAX(ray_depth) AS depth
                        FROM all_rays
                        GROUP BY pixel_id
                    ) AS last_ray_per_pixel
                    JOIN all_rays
                         ON last_ray_per_pixel.pixel_id = all_rays.pixel_id AND
                            last_ray_per_pixel.depth = all_rays.ray_depth
                    WHERE should_trace = TRUE
                ),
                rays_to_trace AS (
                    SELECT last_ray_per_pixel.*
                    FROM last_ray_per_pixel,
                         settings
                    WHERE ray_depth < max_depth
                ),
                rays_exceeding_depth_limit AS (
                    SELECT last_ray_per_pixel.*
                    FROM last_ray_per_pixel,
                         settings
                    WHERE ray_depth >= max_depth
                ),
                closest_ray_intersections AS (
                    WITH
                        sphere_intersection_calc AS (
                            SELECT rays_to_trace.*,
                                   spheres.*,
                                   ray_direction_x * ray_direction_x + ray_direction_y * ray_direction_y +
                                   ray_direction_z * ray_direction_z                  AS a,
                                   (ray_origin_x - sphere_center_x) * ray_direction_x +
                                   (ray_origin_y - sphere_center_y) * ray_direction_y +
                                   (ray_origin_z - sphere_center_z) * ray_direction_z AS half_b,
                                   ((ray_origin_x - sphere_center_x) * (ray_origin_x - sphere_center_x) +
                                    (ray_origin_y - sphere_center_y) * (ray_origin_y - sphere_center_y) +
                                    (ray_origin_z - sphere_center_z) * (ray_origin_z - sphere_center_z)) -
                                   sphere_radius * sphere_radius                      AS c
                            FROM rays_to_trace,
                                 spheres
                        ),
                        sphere_intersection_discriminants AS (
                            SELECT *,
                                   half_b * half_b - a * c AS discriminant
                            FROM sphere_intersection_calc
                        ),
                        sphere_intersection_roots AS (
                            SELECT *,
                                   (-half_b - SQRT(discriminant)) / a AS root_1,
                                   (-half_b + SQRT(discriminant)) / a AS root_2
                            FROM sphere_intersection_discriminants
                            WHERE discriminant >= 0
                        ),
                        sphere_intersection_t AS (
                            SELECT *,
                                   CASE
                                       WHEN root_1 > sphere_collision_t_min AND root_1 <= root_2 THEN root_1
                                       WHEN root_2 > sphere_collision_t_min AND root_2 < root_1 THEN root_2
                                       ELSE -1.0
                                       END AS t
                            FROM sphere_intersection_roots,
                                 settings
                        ),
                        closest_sphere_intersection AS (
                            SELECT sphere_intersection_t.*
                            FROM sphere_intersection_t
                            JOIN (
                                SELECT ray_id, MIN(t) AS t
                                FROM sphere_intersection_t
                                WHERE t >= 0.0
                                GROUP BY ray_id
                            ) lowest_t ON sphere_intersection_t.ray_id = lowest_t.ray_id AND
                                          sphere_intersection_t.t = lowest_t.t
                        )
                    SELECT *
                    FROM closest_sphere_intersection
                ),
                hit_records (pixel_id, ray_id, ray_depth, normal_x, normal_y, normal_z) AS (
                    WITH
                        intersection_point AS (
                            SELECT *,
                                   ray_origin_x + ray_direction_x * t AS point_x,
                                   ray_origin_y + ray_direction_y * t AS point_y,
                                   ray_origin_z + ray_direction_z * t AS point_z
                            FROM closest_ray_intersections
                        ),
                        intersection_outward_normal AS (
                            SELECT intersection_point.*,
                                   (point_x - sphere_center_x) / sphere_radius AS normal_x,
                                   (point_y - sphere_center_y) / sphere_radius AS normal_y,
                                   (point_z - sphere_center_z) / sphere_radius AS normal_z
                            FROM intersection_point
                        ),
                        intersection_is_front_face AS (
                            SELECT *,
                                   ray_direction_x * normal_x + ray_direction_y * normal_y +
                                   ray_direction_z * normal_z <
                                   0.0 AS is_front_face
                            FROM intersection_outward_normal
                        ),
                        intersections (pixel_id, ray_id, ray_depth, sphere_id, t, point_x, point_y, point_z, normal_x,
                                       normal_y, normal_z,
                                       is_front_face)
                            AS (
                            SELECT pixel_id,
                                   ray_id,
                                   ray_depth,
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
                           ray_id,
                           ray_depth,
                           normal_x,
                           normal_y,
                           normal_z
                    FROM intersections
                ),
                new_rays AS (
                    (
                        SELECT pixel_id,
                               ray_id + 1             AS ray_id,
                               ray_depth + 1          AS ray_depth,
                               FALSE                  AS should_trace,
                               0,
                               0,
                               0,
                               0,
                               0,
                               0,
                               (normal_x + 1.0) * 0.5 AS ray_color_r,
                               (normal_y + 1.0) * 0.5 AS ray_color_g,
                               (normal_z + 1.0) * 0.5 AS ray_color_b
                        FROM hit_records
                    )
                    UNION ALL
                    (
                        SELECT pixel_id,
                               ray_id + 1,
                               ray_depth + 1,
                               FALSE,
                               0,
                               0,
                               0,
                               0,
                               0,
                               0,
                               0,
                               0,
                               0
                        FROM rays_exceeding_depth_limit
                    )
                )
            SELECT *
            FROM new_rays
        ) AS _
    ),
    pixels (pixel_id, r, g, b) AS (
        SELECT rays.pixel_id,
               FLOOR(ray_color_r * 0xFF),
               FLOOR(ray_color_g * 0xFF),
               FLOOR(ray_color_b * 0xFF)
        FROM (
            SELECT pixel_id,
                   MAX(ray_depth) AS depth
            FROM rays
            GROUP BY pixel_id
        ) AS last_ray
        JOIN rays ON last_ray.pixel_id = rays.pixel_id AND last_ray.depth = rays.ray_depth
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
