CREATE EXTENSION pgcrypto;
CREATE EXTENSION "uuid-ossp";

CREATE TABLE items (
    created_at timestamp with time zone,
    idata jsonb NOT NULL DEFAULT '{}'::jsonb,
    slug text PRIMARY KEY,
    tags text[] NOT NULL DEFAULT '{}'::text[],
    title text,
    itype text,
    visibility text
);

CREATE INDEX item_slug_visibility_index ON items (slug, visibility);
CREATE INDEX item_tags_visibility_index ON items (tags, visibility);
CREATE INDEX item_visibility_index ON items (visibility);

CREATE TABLE users (
    id SERIAL PRIMARY KEY,
    username text NOT NULL UNIQUE,
    password text,
    role text
);


CREATE TABLE tokens (
    access_token uuid NOT NULL DEFAULT uuid_generate_v4(),
    user_id integer REFERENCES users(id),
    created_at timestamp with time zone DEFAULT now(),
    PRIMARY KEY (access_token, user_id)
);

CREATE INDEX token_created_at_index ON tokens (created_at);
