ALTER TABLE user_roles
ALTER COLUMN role_id TYPE VARCHAR;

INSERT INTO user_roles (identifier, user_identifier, role_id, created_at)
VALUES ('bbb72ccc-37fb-46c0-947a-44a2b576bf9c', 'cab727b8-37fb-46c0-947a-44a2b576bf9c', 'wm::superuser', '2023-07-15 22:02:32.687 +0200');
