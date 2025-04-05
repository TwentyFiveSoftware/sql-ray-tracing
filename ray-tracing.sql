-- MySQL:
-- SET SESSION cte_max_recursion_depth = 1000000;
-- SET SESSION group_concat_max_len = 1000000000000;

-- render
WITH
    RECURSIVE
    settings AS (
        SELECT 800   AS width,
               600   AS height,
               10    AS max_depth,
               0.0   AS camera_look_from_x,
               0.0   AS camera_look_from_y,
               -4.0  AS camera_look_from_z,
               0.0   AS camera_look_at_x,
               0.0   AS camera_look_at_y,
               0.0   AS camera_look_at_z,
               25.0  AS camera_fov,
               0.001 AS sphere_collision_t_min
    ),
    spheres (sphere_id, sphere_center_x, sphere_center_y, sphere_center_z, sphere_radius, sphere_material_type,
             sphere_albedo_r, sphere_albedo_g, sphere_albedo_b) AS (
        SELECT 0,
               0,
               0,
               1,
               0.5,
               'DIFFUSE',
               0.9,
               0.9,
               0.9
        UNION ALL
        SELECT 1,
               0,
               -1000.5,
               0,
               1000,
               'DIFFUSE',
               0.3,
               0.3,
               0.3
    ),
    camera AS (
        WITH
            camera_viewport_height (viewport_height) AS (
                SELECT TAN(RADIANS(camera_fov) / 2.0) * 2.0
                FROM settings
            ),
            camera_viewport_width (viewport_width) AS (
                SELECT CAST(width AS FLOAT) / height * viewport_height AS viewport_width
                FROM settings,
                     camera_viewport_height
            ),
            camera_forward (forward_x, forward_y, forward_z) AS (
                WITH
                    forward_non_normalized (forward_x, forward_y, forward_z) AS (
                        SELECT camera_look_at_x - camera_look_from_x,
                               camera_look_at_y - camera_look_from_y,
                               camera_look_at_z - camera_look_from_z
                        FROM settings
                    )
                SELECT forward_x / SQRT(forward_x * forward_x + forward_y * forward_y + forward_z * forward_z),
                       forward_y / SQRT(forward_x * forward_x + forward_y * forward_y + forward_z * forward_z),
                       forward_z / SQRT(forward_x * forward_x + forward_y * forward_y + forward_z * forward_z)
                FROM forward_non_normalized
            ),
            camera_right (right_x, right_y, right_z) AS (
                WITH
                    right_non_normalized (right_x, right_y, right_z) AS (
                        SELECT forward_z, 0, - forward_x
                        FROM camera_forward
                    )
                SELECT right_x / SQRT(right_x * right_x + right_y * right_y + right_z * right_z),
                       right_y / SQRT(right_x * right_x + right_y * right_y + right_z * right_z),
                       right_z / SQRT(right_x * right_x + right_y * right_y + right_z * right_z)
                FROM right_non_normalized
            ),
            camera_up (up_x, up_y, up_z) AS (
                WITH
                    up_non_normalized (up_x, up_y, up_z) AS (
                        SELECT (forward_y * right_z) - (forward_z * right_y),
                               (forward_z * right_x) - (forward_x * right_z),
                               (forward_x * right_y) - (forward_y * right_x)
                        FROM camera_forward,
                             camera_right
                    )
                SELECT up_x / (up_x * up_x + up_y * up_y + up_z * up_z),
                       up_y / (up_x * up_x + up_y * up_y + up_z * up_z),
                       up_z / (up_x * up_x + up_y * up_y + up_z * up_z)
                FROM up_non_normalized
            ),
            camera_directions (horizontal_direction_x, horizontal_direction_y, horizontal_direction_z,
                               vertical_direction_x, vertical_direction_y, vertical_direction_z) AS (
                SELECT right_x * viewport_width,
                       right_y * viewport_width,
                       right_z * viewport_width,
                       up_x * viewport_height,
                       up_y * viewport_height,
                       up_z * viewport_height
                FROM camera_viewport_width,
                     camera_viewport_height,
                     camera_right,
                     camera_up
            ),
            camera_upper_left_corner (upper_left_corner_x, upper_left_corner_y, upper_left_corner_z) AS (
                SELECT camera_look_from_x - horizontal_direction_x / 2.0 + vertical_direction_x / 2.0 + forward_x,
                       camera_look_from_y - horizontal_direction_y / 2.0 + vertical_direction_y / 2.0 + forward_y,
                       camera_look_from_z - horizontal_direction_z / 2.0 + vertical_direction_z / 2.0 + forward_z
                FROM settings,
                     camera_directions,
                     camera_forward
            )
        SELECT camera_directions.*, camera_upper_left_corner.*
        FROM camera_directions,
             camera_upper_left_corner
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
            camera_rays AS (
                WITH
                    non_normalized_camera_ray_directions (pixel_id, ray_direction_x, ray_direction_y, ray_direction_z)
                        AS (
                        SELECT pixel_id,
                               upper_left_corner_x + horizontal_direction_x * u - vertical_direction_x * v -
                               camera_look_from_x,
                               upper_left_corner_y + horizontal_direction_y * u - vertical_direction_y * v -
                               camera_look_from_y,
                               upper_left_corner_z + horizontal_direction_z * u - vertical_direction_z * v -
                               camera_look_from_z
                        FROM pixel_coordinates,
                             camera,
                             settings
                    )
                SELECT pixel_id,
                       pixel_id * (max_depth + 1)                                AS ray_id,
                       0                                                         AS ray_depth,
                       TRUE                                                      AS should_trace,
                       CAST(camera_look_from_x AS FLOAT)                         AS ray_origin_x,
                       CAST(camera_look_from_y AS FLOAT)                         AS ray_origin_y,
                       CAST(camera_look_from_z AS FLOAT)                         AS ray_origin_z,
                       ray_direction_x / SQRT(ray_direction_x * ray_direction_x + ray_direction_y * ray_direction_y +
                                              ray_direction_z * ray_direction_z) AS ray_direction_x,
                       ray_direction_y / SQRT(ray_direction_x * ray_direction_x + ray_direction_y * ray_direction_y +
                                              ray_direction_z * ray_direction_z) AS ray_direction_y,
                       ray_direction_z / SQRT(ray_direction_x * ray_direction_x + ray_direction_y * ray_direction_y +
                                              ray_direction_z * ray_direction_z) AS ray_direction_z
                FROM non_normalized_camera_ray_directions,
                     settings
            ),
            camera_rays_with_background_color AS (
                SELECT *,
                       1.0 * (1.0 - ((ray_direction_y + 1.0) * 0.5))
                           + 0.3 * ((ray_direction_y + 1.0) * 0.5) AS ray_color_r,
                       1.0 * (1.0 - ((ray_direction_y + 1.0) * 0.5))
                           + 0.5 * ((ray_direction_y + 1.0) * 0.5) AS ray_color_g,
                       1.0 * (1.0 - ((ray_direction_y + 1.0) * 0.5))
                           + 0.8 * ((ray_direction_y + 1.0) * 0.5) AS ray_color_b
                FROM camera_rays
            )
        SELECT *
        FROM camera_rays_with_background_color

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
                    SELECT pixel_id, ray_id, ray_depth
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
                            -- Postgres only, but much faster:
                            SELECT DISTINCT ON (ray_id) *
                            FROM sphere_intersection_t
                            WHERE t >= 0.0
                            ORDER BY ray_id, t

--                             SELECT sphere_intersection_t.*
--                             FROM sphere_intersection_t
--                             JOIN (
--                                 SELECT ray_id, MIN(t) AS t
--                                 FROM sphere_intersection_t
--                                 WHERE t >= 0.0
--                                 GROUP BY ray_id
--                             ) lowest_t ON sphere_intersection_t.ray_id = lowest_t.ray_id AND
--                                           sphere_intersection_t.t = lowest_t.t
                        )
                    SELECT *
                    FROM closest_sphere_intersection
                ),
                hit_records AS (
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
                                   (point_x - sphere_center_x) / sphere_radius AS outward_normal_x,
                                   (point_y - sphere_center_y) / sphere_radius AS outward_normal_y,
                                   (point_z - sphere_center_z) / sphere_radius AS outward_normal_z
                            FROM intersection_point
                        ),
                        intersection_is_front_face AS (
                            SELECT *,
                                   ray_direction_x * outward_normal_x + ray_direction_y * outward_normal_y +
                                   ray_direction_z * outward_normal_z <
                                   0.0 AS is_front_face
                            FROM intersection_outward_normal
                        ),
                        intersections AS (
                            SELECT *,
                                   CASE WHEN is_front_face THEN outward_normal_x ELSE -outward_normal_x END AS normal_x,
                                   CASE WHEN is_front_face THEN outward_normal_y ELSE -outward_normal_y END AS normal_y,
                                   CASE WHEN is_front_face THEN outward_normal_z ELSE -outward_normal_z END AS normal_z
                            FROM intersection_is_front_face
                        )
                    SELECT *
                    FROM intersections
                ),
                scattered_diffuse_rays AS (
                    WITH
                        RECURSIVE
                        incomplete_scattered_rays AS (
                            SELECT pixel_id,
                                   ray_id + 1                    AS ray_id,
                                   ray_depth + 1                 AS ray_depth,
                                   point_x                       AS ray_origin_x,
                                   point_y                       AS ray_origin_y,
                                   point_z                       AS ray_origin_z,
                                   ray_color_r * sphere_albedo_r AS ray_color_r,
                                   ray_color_g * sphere_albedo_g AS ray_color_g,
                                   ray_color_b * sphere_albedo_b AS ray_color_b,
                                   normal_x                      AS hit_normal_x,
                                   normal_y                      AS hit_normal_y,
                                   normal_z                      AS hit_normal_z
                            FROM hit_records
                            WHERE sphere_material_type = 'DIFFUSE'
                        ),
                        random_unit_vectors AS (
                            (
                                SELECT *,
                                       RANDOM() * 2.0 - 1.0 AS x,
                                       RANDOM() * 2.0 - 1.0 AS y,
                                       RANDOM() * 2.0 - 1.0 AS z
                                FROM incomplete_scattered_rays
                            )
                            UNION ALL
                            (
                                SELECT pixel_id,
                                       ray_id,
                                       ray_depth,
                                       ray_origin_x,
                                       ray_origin_y,
                                       ray_origin_z,
                                       ray_color_r,
                                       ray_color_g,
                                       ray_color_b,
                                       hit_normal_x,
                                       hit_normal_y,
                                       hit_normal_z,
                                       RANDOM() * 2.0 - 1.0 AS x,
                                       RANDOM() * 2.0 - 1.0 AS y,
                                       RANDOM() * 2.0 - 1.0 AS z
                                FROM random_unit_vectors
                                WHERE x * x + y * y + z * z > 1
                                   OR x + y + z = 0
                            )
                        ),
                        normalized_random_unit_vectors AS (
                            -- Postgres only:
                            SELECT DISTINCT ON (ray_id) *,
                                                        x / SQRT(x * x + y * y + z * z) AS random_unit_vector_x,
                                                        y / SQRT(x * x + y * y + z * z) AS random_unit_vector_y,
                                                        z / SQRT(x * x + y * y + z * z) AS random_unit_vector_z
                            FROM random_unit_vectors
                            WHERE x * x + y * y + z * z <= 1
                              AND x + y + z <> 0
                        ),
                        scatter_records_with_non_normalized_scatter_ray_direction AS (
                            SELECT *,
                                   hit_normal_x + random_unit_vector_x AS direction_x,
                                   hit_normal_y + random_unit_vector_y AS direction_y,
                                   hit_normal_z + random_unit_vector_z AS direction_z
                            FROM normalized_random_unit_vectors
                        ),
                        scatter_records_with_scatter_ray_direction AS (
                            SELECT *,
                                   direction_x / SQRT(direction_x * direction_x + direction_y * direction_y +
                                                      direction_z * direction_z) AS ray_direction_x,
                                   direction_y / SQRT(direction_x * direction_x + direction_y * direction_y +
                                                      direction_z * direction_z) AS ray_direction_y,
                                   direction_z / SQRT(direction_x * direction_x + direction_y * direction_y +
                                                      direction_z * direction_z) AS ray_direction_z
                            FROM scatter_records_with_non_normalized_scatter_ray_direction
                        )
                    SELECT pixel_id,
                           ray_id,
                           ray_depth,
                           TRUE AS should_trace,
                           ray_origin_x,
                           ray_origin_y,
                           ray_origin_z,
                           ray_direction_x,
                           ray_direction_y,
                           ray_direction_z,
                           ray_color_r,
                           ray_color_g,
                           ray_color_b
                    FROM scatter_records_with_scatter_ray_direction
                ),
                new_rays AS (
                    SELECT *
                    FROM scattered_diffuse_rays
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
                        FROM (
                            SELECT pixel_id, ray_id, ray_depth
                            FROM rays_exceeding_depth_limit
                            UNION ALL
                            SELECT pixel_id, ray_id, ray_depth
                            FROM hit_records
                            WHERE sphere_material_type <> 'DIFFUSE'
                        ) AS _
                    )
                )
            SELECT *
            FROM new_rays
        ) AS _
    ),
    pixels (pixel_id, r, g, b) AS (
        -- Postgres only, but much faster:
        SELECT DISTINCT ON (pixel_id) pixel_id,
                                      FLOOR(ray_color_r * 0xFF),
                                      FLOOR(ray_color_g * 0xFF),
                                      FLOOR(ray_color_b * 0xFF)
        FROM rays
        ORDER BY pixel_id, ray_depth DESC

--         SELECT rays.pixel_id,
--                FLOOR(ray_color_r * 0xFF),
--                FLOOR(ray_color_g * 0xFF),
--                FLOOR(ray_color_b * 0xFF)
--         FROM (
--             SELECT pixel_id,
--                    MAX(ray_depth) AS depth
--             FROM rays
--             GROUP BY pixel_id
--         ) AS last_ray
--         JOIN rays ON last_ray.pixel_id = rays.pixel_id AND last_ray.depth = rays.ray_depth
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
