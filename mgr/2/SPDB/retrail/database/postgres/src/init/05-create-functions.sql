-- Connect to proper database
\c retrail

CREATE OR REPLACE VIEW ways_cost AS SELECT gid as id, source, target, cost, reverse_cost FROM ways;
CREATE OR REPLACE VIEW ways_cost_time AS SELECT gid as id, source, target, cost_s as cost, reverse_cost_s as reverse_cost FROM ways;

create type route as (  
	_seq	INTEGER,
	_x 	numeric(11,8),
	_y 	numeric(11,8), 
	_cost_from 	FLOAT
); 

-- for given coordinates, return coordinates of the closest point in topology 
-- (approximate, not actually on any line)
CREATE OR REPLACE FUNCTION closest_point(x_query DOUBLE PRECISION, y_query DOUBLE PRECISION)
RETURNS TABLE (
	x DOUBLE PRECISION,
	y DOUBLE PRECISION
) 
AS
$$
BEGIN
RETURN QUERY SELECT
point_x, point_y
FROM
(SELECT ST_X(cp) as point_x, ST_Y(cp) as point_y
FROM
(SELECT ST_ClosestPoint(w.the_geom, p.st_setsrid) as cp, ST_Distance(w.the_geom, p.st_setsrid) as dist
FROM ways AS w, (SELECT ST_SetSRID(ST_MakePoint(x_query, y_query), 4326)) AS p
ORDER BY dist ASC
FETCH FIRST ROW ONLY) cp) coords;
END
$$ LANGUAGE plpgsql;


-- for given coordinates, return coordinates of the closest node
CREATE OR REPLACE FUNCTION closest_node(x_query DOUBLE PRECISION, y_query DOUBLE PRECISION)
RETURNS TABLE (
	x DOUBLE PRECISION,
	y DOUBLE PRECISION
) 
AS
$$
BEGIN
RETURN QUERY SELECT
point_x, point_y
FROM
(SELECT ST_X(cp) as point_x, ST_Y(cp) as point_y
FROM
(SELECT ST_ClosestPoint(v.the_geom, p.st_setsrid) as cp, ST_Distance(v.the_geom, p.st_setsrid) as dist
FROM ways_vertices_pgr AS v, (SELECT ST_SetSRID(ST_MakePoint(x_query, y_query), 4326)) AS p
ORDER BY dist ASC
FETCH FIRST ROW ONLY) cp) coords;
END
$$ LANGUAGE plpgsql;


-- for given coordinates, return geometry of the point that they specify 
-- and id + geometry of the way closest to that point
CREATE OR REPLACE FUNCTION closest_geom(x_query DOUBLE PRECISION, y_query DOUBLE PRECISION)
RETURNS TABLE (
	newpt_geom GEOMETRY,
	way_gid BIGINT,
	way_geom GEOMETRY
)
AS
$$
BEGIN
RETURN QUERY SELECT
cp.newptgeom, cp.wgid, cp.wgeom
FROM
(SELECT p.st_setsrid as newptgeom, w.gid as wgid, w.the_geom as wgeom, ST_Distance(w.the_geom, p.st_setsrid) as dist
FROM ways AS w, (SELECT ST_SetSRID(ST_MakePoint(x_query, y_query), 4326)) as p
ORDER BY dist ASC) cp
FETCH FIRST ROW ONLY;
END
$$ LANGUAGE plpgsql;


-- used by find_path
-- can be used for checking results of trsp

CREATE OR REPLACE FUNCTION trsp_find_path(src_x DOUBLE PRECISION, src_y DOUBLE PRECISION, trg_x DOUBLE PRECISION, trg_y DOUBLE PRECISION, by_time BOOLEAN default false)
RETURNS TABLE (
	_seq	INTEGER, 
	_id1	INTEGER, 
	_id2	INTEGER,
	_x 	numeric(11,8),
	_y 	numeric(11,8), 
	_cost_from 	FLOAT,
	_geom	GEOMETRY
)
AS
$$
DECLARE
src_way_gid	BIGINT;
trg_way_gid	BIGINT;
src_pt_geom	GEOMETRY;
trg_pt_geom	GEOMETRY;
src_way_geom	GEOMETRY;
trg_way_geom	GEOMETRY;
ways_sql	TEXT;
BEGIN

if by_time then
   SELECT 'SELECT id::INTEGER, source::INTEGER, target::INTEGER, cost, reverse_cost FROM ways_cost_time' INTO ways_sql;
else
  SELECT 'SELECT id::INTEGER, source::INTEGER, target::INTEGER, cost, reverse_cost FROM ways_cost' INTO ways_sql;
END if;

SELECT way_gid, newpt_geom, way_geom INTO src_way_gid, src_pt_geom, src_way_geom
FROM closest_geom(src_x, src_y);

SELECT way_gid, newpt_geom, way_geom INTO trg_way_gid, trg_pt_geom, trg_way_geom
FROM closest_geom(trg_x, trg_y);

RETURN QUERY SELECT
seq, id1, id2, lon, lat, cost, the_geom
FROM pgr_trsp(ways_sql, 
			  src_way_gid::INTEGER, 
			  ST_LineLocatePoint(src_way_geom, src_pt_geom)::FLOAT,
			  trg_way_gid::INTEGER, 
			  ST_LineLocatePoint(trg_way_geom, trg_pt_geom)::FLOAT,
			  true, true) as trsp 
			  LEFT JOIN ways_vertices_pgr ON (ways_vertices_pgr.id = trsp.id1)
			  ORDER BY seq ASC;

END
$$ LANGUAGE plpgsql;


-- for the given pair of coordinates, return a path as a table of ordered points
-- where every row defines a point with:
-- longitude, latitude, cost of going to the next point in path
-- parameter by_time: if true, take time as cost; if false, take distance as cost
-- example: SELECT * FROM find_path(20.922122, 52.229610, 21.105709, 52.238457, true); 

CREATE OR REPLACE FUNCTION find_path(src_x DOUBLE PRECISION, src_y DOUBLE PRECISION, trg_x DOUBLE PRECISION, trg_y DOUBLE PRECISION, by_time BOOLEAN default false)
RETURNS TABLE (
	x 	numeric(11,8),
	y 	numeric(11,8), 
	cost_from 	FLOAT
)
AS
$$
DECLARE
routes		route[] = ARRAY[]::route[];
full_routes	route[] = ARRAY[]::route[];
r_count		INTEGER;
BEGIN

SELECT array_agg(row(_seq, _x, _y, _cost_from)::route) into routes from trsp_find_path(src_x, src_y, trg_x, trg_y, by_time);
SELECT array_length(routes, 1) INTO r_count;

routes[1]._x = src_x;
routes[1]._y = src_y;

if routes[r_count]._x then
	routes := routes || (r_count, trg_x, trg_y, 0)::route;	
else
	routes[r_count]._x = trg_x;
	routes[r_count]._y = trg_y;
END if;

RETURN QUERY SELECT _x as x, _y as y, _cost_from as cost_from FROM unnest(routes);
END
$$ LANGUAGE plpgsql;
