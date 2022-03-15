-- Must reflect order and naming in Common.Api.Role
INSERT INTO roles(id, name)
VALUES (0, 'Basic'), (1, 'Contributor'), (2, 'Moderator'), (3, 'Administrator')
ON CONFLICT DO NOTHING;
