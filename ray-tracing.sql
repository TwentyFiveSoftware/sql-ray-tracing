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

# EXPLAIN ANALYZE
WITH
    RECURSIVE
    spheres (center_x, center_y, center_z, radius) AS (
        SELECT 0, 0, 1, 0.5
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
    not_normalized_rays (ray_id, pixel_x, pixel_y, origin_x, origin_y, origin_z, direction_x, direction_y, direction_z)
        AS (
        SELECT y * @WIDTH + x,
               x,
               y,
               @CAMERA_ORIGIN_X,
               @CAMERA_ORIGIN_Y,
               @CAMERA_ORIGIN_Z,
               @CAMERA_UPPER_LEFT_CORNER_X + @VIEWPORT_WIDTH * u,
               @CAMERA_UPPER_LEFT_CORNER_Y + @VIEWPORT_HEIGHT * v,
               @CAMERA_UPPER_LEFT_CORNER_Z
        FROM pixel_uv
    ),
    rays (ray_id, pixel_x, pixel_y, origin_x, origin_y, origin_z, direction_x, direction_y, direction_z) AS (
        SELECT ray_id,
               pixel_x,
               pixel_y,
               origin_x,
               origin_y,
               origin_z,
               direction_x / SQRT(direction_x * direction_x + direction_y * direction_y + direction_z * direction_z),
               direction_y / SQRT(direction_x * direction_x + direction_y * direction_y + direction_z * direction_z),
               direction_z / SQRT(direction_x * direction_x + direction_y * direction_y + direction_z * direction_z)
        FROM not_normalized_rays
    ),
    ray_sphere_collisions (ray_id, t, point_x, point_y, point_z, normal_x, normal_y, normal_z, is_front_facing)
        AS (
        SELECT -1, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, FALSE -- TODO: TEMP!
#         SELECT ray_id,
#                0.0,
#                0.0,
#                0.0,
#                0.0,
#                0.0,
#                0.0,
#                0.0,
#                FALSE
#         FROM rays
    ),
    ray_collisions (ray_id, t, point_x, point_y, point_z, normal_x, normal_y, normal_z, is_front_facing) AS (
        SELECT ray_sphere_collisions.ray_id,
               ray_sphere_collisions.t,
               point_x,
               point_y,
               point_z,
               normal_x,
               normal_y,
               normal_z,
               is_front_facing
        FROM ray_sphere_collisions
        JOIN (
            SELECT ray_id, MIN(t) AS t
            FROM ray_sphere_collisions
            GROUP BY ray_id
        ) lowest_t ON ray_sphere_collisions.ray_id = lowest_t.ray_id AND ray_sphere_collisions.t = lowest_t.t
    ),
    ray_hit_records (pixel_x, pixel_y, ray_direction_y, normal_x, normal_y, normal_z) AS (
        SELECT pixel_x,
               pixel_y,
               direction_y,
               normal_x,
               normal_y,
               normal_z
        FROM rays
        LEFT OUTER JOIN ray_sphere_collisions ON rays.ray_id = ray_sphere_collisions.ray_id
    ),
    ray_colors (pixel_x, pixel_y, r, g, b) AS (
        SELECT pixel_x,
               pixel_y,
               COALESCE(normal_x,
                        1.0 * (1.0 - ((ray_direction_y + 1.0) * 0.5)) + 0.3 * ((ray_direction_y + 1.0) * 0.5)),
               COALESCE(normal_y,
                        1.0 * (1.0 - ((ray_direction_y + 1.0) * 0.5)) + 0.5 * ((ray_direction_y + 1.0) * 0.5)),
               COALESCE(normal_z,
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
