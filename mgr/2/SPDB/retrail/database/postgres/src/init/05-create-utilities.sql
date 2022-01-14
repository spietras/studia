-- Connect to proper database
\c retrail

-- Create functions

CREATE FUNCTION closest(x_query NUMERIC, y_query NUMERIC)
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
