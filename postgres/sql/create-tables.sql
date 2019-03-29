DROP TABLE IF EXISTS child;
DROP TABLE IF EXISTS parent;

CREATE TABLE parent (
    id serial PRIMARY KEY,
    name text NOT NULL,
    description text NOT NULL
);

CREATE TABLE child (
    id serial PRIMARY KEY,
    parent_id integer NOT NULL REFERENCES parent (id) ON DELETE CASCADE,
    value double precision NOT NULL
);