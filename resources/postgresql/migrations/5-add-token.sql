CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

ALTER TABLE users ADD COLUMN auth_token UUID NULL DEFAULT uuid_generate_v4();
