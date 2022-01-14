-- Connect to proper database
\c retrail

-- Create functions

CREATE FUNCTION closest(
    x_query NUMERIC,
    y_query NUMERIC
)
    RETURNS TABLE
            (
                x NUMERIC,
                y NUMERIC
            )
AS
$$
BEGIN
    RETURN QUERY
        SELECT x_query AS x, y_query AS y; -- TODO
END
$$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION find_path(
    x1 NUMERIC,
    y1 NUMERIC,
    x2 NUMERIC,
    y2 NUMERIC
)
    RETURNS TABLE
            (
                x    NUMERIC,
                y    NUMERIC,
                cost NUMERIC
            )
AS
$$
BEGIN
    RETURN QUERY
        SELECT x1 as x, y1 as y, 0.0 as cost
        UNION ALL
        SELECT x2 as x, y2 as y, 1.0 as cost;
END
$$ LANGUAGE plpgsql;
