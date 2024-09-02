
CREATE TABLE users(
identifier UUID NOT NULL,
display_name VARCHAR(340) NOT NULL,
email_address VARCHAR NOT NULL,
password_hash VARCHAR NOT NULL,
password_reset_token VARCHAR NULL,
latest_login_at TIMESTAMPTZ NULL,
latest_login_device VARCHAR NULL,
avatar_url VARCHAR NULL,
created_at TIMESTAMPTZ NOT NULL,
last_edited_at TIMESTAMPTZ NULL,
UNIQUE (identifier),
PRIMARY KEY (identifier)
);

CREATE TABLE artists(
identifier UUID NOT NULL,
display_name VARCHAR(340) NOT NULL,
created_by UUID NOT NULL REFERENCES users (identifier),
visibility_status INT NOT NULL,
approved_by UUID NULL REFERENCES users (identifier),
created_at TIMESTAMPTZ NOT NULL,
last_edited_at TIMESTAMPTZ NULL,
UNIQUE (identifier),
PRIMARY KEY (identifier)
);

CREATE TABLE forum_pages(
identifier UUID NOT NULL,
display_name VARCHAR(180) NOT NULL,
created_by UUID NOT NULL REFERENCES users (identifier),
visibility_status INT NOT NULL,
approved_by UUID NULL REFERENCES users (identifier),
created_at TIMESTAMPTZ NOT NULL,
last_edited_at TIMESTAMPTZ NULL,
UNIQUE (identifier),
PRIMARY KEY (identifier)
);

CREATE TABLE forum_categories(
identifier UUID NOT NULL,
parent_identifier UUID NULL REFERENCES forum_categories (identifier),
display_name VARCHAR(340) NOT NULL,
created_by UUID NOT NULL REFERENCES users (identifier),
visibility_status INT NOT NULL,
approved_by UUID NULL REFERENCES users (identifier),
created_at TIMESTAMPTZ NOT NULL,
last_edited_at TIMESTAMPTZ NULL,
UNIQUE (identifier),
PRIMARY KEY (identifier)
);

CREATE TABLE genres(
identifier UUID NOT NULL,
parent_identifier UUID NULL REFERENCES genres (identifier),
display_name VARCHAR(340) NOT NULL,
created_by UUID NOT NULL REFERENCES users (identifier),
visibility_status INT NOT NULL,
approved_by UUID NULL REFERENCES users (identifier),
created_at TIMESTAMPTZ NOT NULL,
last_edited_at TIMESTAMPTZ NULL,
UNIQUE (identifier),
PRIMARY KEY (identifier)
);

CREATE TABLE songs(
identifier UUID NOT NULL,
display_name VARCHAR(340) NOT NULL,
music_key VARCHAR(100) NULL,
music_tuning VARCHAR(100) NULL,
music_creation_date VARCHAR(340) NULL,
album_name VARCHAR(340) NULL,
album_info_link VARCHAR NULL,
created_by UUID NOT NULL REFERENCES users (identifier),
visibility_status INT NOT NULL,
approved_by UUID NULL REFERENCES users (identifier),
created_at TIMESTAMPTZ NOT NULL,
last_edited_at TIMESTAMPTZ NULL,
UNIQUE (identifier),
PRIMARY KEY (identifier)
);

CREATE TABLE user_favourite_song_folders(
identifier UUID NOT NULL,
user_identifier UUID NOT NULL REFERENCES users (identifier),
song_folder_name VARCHAR NOT NULL,
created_at TIMESTAMPTZ NOT NULL,
last_edited_at TIMESTAMPTZ NULL,
UNIQUE(user_identifier, song_folder_name),
UNIQUE (identifier),
PRIMARY KEY (identifier)
);


CREATE TABLE artist_artworks(
identifier UUID NOT NULL,
artist_identifier UUID NOT NULL REFERENCES artists (identifier),
created_by UUID NOT NULL REFERENCES users (identifier),
visibility_status INT NOT NULL,
approved_by UUID NULL REFERENCES users (identifier),
content_url VARCHAR NOT NULL,
content_caption VARCHAR(8200) NULL,
order_value INT NOT NULL,
created_at TIMESTAMPTZ NOT NULL,
last_edited_at TIMESTAMPTZ NULL,
UNIQUE (identifier),
PRIMARY KEY (identifier)
);

CREATE TABLE artist_comments(
identifier UUID NOT NULL,
parent_identifier UUID NULL REFERENCES artist_comments (identifier),
artist_identifier UUID NOT NULL REFERENCES artists (identifier),
created_by UUID NOT NULL REFERENCES users (identifier),
visibility_status INT NOT NULL,
contents VARCHAR(8200) NOT NULL,
approved_by UUID NULL REFERENCES users (identifier),
created_at TIMESTAMPTZ NOT NULL,
last_edited_at TIMESTAMPTZ NULL,
UNIQUE (identifier),
PRIMARY KEY (identifier)
);

CREATE TABLE artist_external_sources(
identifier UUID NOT NULL,
artist_identifier UUID NOT NULL REFERENCES artists (identifier),
created_by UUID NOT NULL REFERENCES users (identifier),
spotify_url VARCHAR NULL,
youtube_url VARCHAR NULL,
soundcloud_url VARCHAR NULL,
wikipedia_url VARCHAR NULL,
created_at TIMESTAMPTZ NOT NULL,
last_edited_at TIMESTAMPTZ NULL,
UNIQUE (artist_identifier),
UNIQUE (identifier),
PRIMARY KEY (identifier)
);

CREATE TABLE artist_opinions(
identifier UUID NOT NULL,
artist_identifier UUID NOT NULL REFERENCES artists (identifier),
created_by UUID NOT NULL REFERENCES users (identifier),
is_like BOOLEAN NOT NULL,
is_dislike BOOLEAN NOT NULL,
created_at TIMESTAMPTZ NOT NULL,
last_edited_at TIMESTAMPTZ NULL,
UNIQUE(artist_identifier, created_by),
UNIQUE (identifier),
PRIMARY KEY (identifier)
);


CREATE TABLE user_favourite_artists(
identifier UUID NOT NULL,
user_identifier UUID NOT NULL REFERENCES users (identifier),
artist_identifier UUID NOT NULL REFERENCES artists (identifier),
created_at TIMESTAMPTZ NOT NULL,
last_edited_at TIMESTAMPTZ NULL,
UNIQUE(user_identifier, artist_identifier),
UNIQUE (identifier),
PRIMARY KEY (identifier)
);

CREATE TABLE forum_page_comments(
identifier UUID NOT NULL,
parent_identifier UUID NULL REFERENCES forum_page_comments (identifier),
forum_page_identifier UUID NOT NULL REFERENCES forum_pages (identifier),
created_by UUID NOT NULL REFERENCES users (identifier),
visibility_status INT NOT NULL,
contents VARCHAR(8200) NOT NULL,
approved_by UUID NULL REFERENCES users (identifier),
created_at TIMESTAMPTZ NOT NULL,
last_edited_at TIMESTAMPTZ NULL,
UNIQUE (identifier),
PRIMARY KEY (identifier)
);

CREATE TABLE forum_page_contents(
identifier UUID NOT NULL,
forum_page_identifier UUID NOT NULL REFERENCES forum_pages (identifier),
created_by UUID NOT NULL REFERENCES users (identifier),
ascii_contents TEXT NULL,
html_contents TEXT NULL,
pdf_contents TEXT NULL,
created_at TIMESTAMPTZ NOT NULL,
last_edited_at TIMESTAMPTZ NULL,
UNIQUE (forum_page_identifier),
UNIQUE (identifier),
PRIMARY KEY (identifier)
);

CREATE TABLE forum_page_external_sources(
identifier UUID NOT NULL,
forum_page_identifier UUID NOT NULL REFERENCES forum_pages (identifier),
created_by UUID NOT NULL REFERENCES users (identifier),
read_more_url VARCHAR NULL,
created_at TIMESTAMPTZ NOT NULL,
last_edited_at TIMESTAMPTZ NULL,
UNIQUE (forum_page_identifier),
UNIQUE (identifier),
PRIMARY KEY (identifier)
);

CREATE TABLE forum_page_opinions(
identifier UUID NOT NULL,
forum_page_identifier UUID NOT NULL REFERENCES forum_pages (identifier),
created_by UUID NOT NULL REFERENCES users (identifier),
is_like BOOLEAN NOT NULL,
is_dislike BOOLEAN NOT NULL,
created_at TIMESTAMPTZ NOT NULL,
last_edited_at TIMESTAMPTZ NULL,
UNIQUE(forum_page_identifier, created_by),
UNIQUE (identifier),
PRIMARY KEY (identifier)
);

CREATE TABLE user_favourite_forum_pages(
identifier UUID NOT NULL,
user_identifier UUID NOT NULL REFERENCES users (identifier),
forum_page_identifier UUID NOT NULL REFERENCES forum_pages (identifier),
created_at TIMESTAMPTZ NOT NULL,
last_edited_at TIMESTAMPTZ NULL,
UNIQUE(user_identifier, forum_page_identifier),
UNIQUE (identifier),
PRIMARY KEY (identifier)
);

CREATE TABLE forum_category_artworks(
identifier UUID NOT NULL,
forum_category_identifier UUID NOT NULL REFERENCES forum_categories (identifier),
created_by UUID NOT NULL REFERENCES users (identifier),
visibility_status INT NOT NULL,
approved_by UUID NULL REFERENCES users (identifier),
content_url VARCHAR NOT NULL,
content_caption VARCHAR(8200),
order_value INT NOT NULL,
created_at TIMESTAMPTZ NOT NULL,
last_edited_at TIMESTAMPTZ NULL,
UNIQUE (identifier),
PRIMARY KEY (identifier)
);

CREATE TABLE forum_category_comments(
identifier UUID NOT NULL,
parent_identifier UUID NULL REFERENCES forum_category_comments (identifier),
forum_category_identifier UUID NOT NULL REFERENCES forum_categories (identifier),
created_by UUID NOT NULL REFERENCES users (identifier),
visibility_status INT NOT NULL,
contents VARCHAR(8200) NOT NULL,
approved_by UUID NULL REFERENCES users (identifier),
created_at TIMESTAMPTZ NOT NULL,
last_edited_at TIMESTAMPTZ NULL,
UNIQUE (identifier),
PRIMARY KEY (identifier)
);

CREATE TABLE forum_category_external_sources(
identifier UUID NOT NULL,
forum_category_identifier UUID NOT NULL REFERENCES forum_categories (identifier),
created_by UUID NOT NULL REFERENCES users (identifier),
read_more_url VARCHAR NULL,
created_at TIMESTAMPTZ NOT NULL,
last_edited_at TIMESTAMPTZ NULL,
UNIQUE (forum_category_identifier),
UNIQUE (identifier),
PRIMARY KEY (identifier)
);

CREATE TABLE forum_category_opinions(
identifier UUID NOT NULL,
forum_category_identifier UUID NOT NULL REFERENCES forum_categories (identifier),
created_by UUID NOT NULL REFERENCES users (identifier),
is_like BOOLEAN NOT NULL,
is_dislike BOOLEAN NOT NULL,
created_at TIMESTAMPTZ NOT NULL,
last_edited_at TIMESTAMPTZ NULL,
UNIQUE(forum_category_identifier, created_by),
UNIQUE (identifier),
PRIMARY KEY (identifier)
);


CREATE TABLE user_favourite_forum_categories(
identifier UUID NOT NULL,
user_identifier UUID NOT NULL REFERENCES users (identifier),
forum_category_identifier UUID NOT NULL REFERENCES forum_categories (identifier),
created_at TIMESTAMPTZ NOT NULL,
last_edited_at TIMESTAMPTZ NULL,
UNIQUE(user_identifier, forum_category_identifier),
UNIQUE (identifier),
PRIMARY KEY (identifier)
);

CREATE TABLE genre_artworks(
identifier UUID NOT NULL,
genre_identifier UUID NOT NULL REFERENCES genres (identifier),
created_by UUID NOT NULL REFERENCES users (identifier),
visibility_status INT NOT NULL,
approved_by UUID NULL REFERENCES users (identifier),
content_url VARCHAR NOT NULL,
content_caption VARCHAR(8200) NULL,
order_value INT NOT NULL,
created_at TIMESTAMPTZ NOT NULL,
last_edited_at TIMESTAMPTZ NULL,
UNIQUE (identifier),
PRIMARY KEY (identifier)
);

CREATE TABLE genre_comments(
identifier UUID NOT NULL,
parent_identifier UUID NULL REFERENCES genre_comments (identifier),
genre_identifier UUID NOT NULL REFERENCES genres (identifier),
created_by UUID NOT NULL REFERENCES users (identifier),
visibility_status INT NOT NULL,
contents VARCHAR(8200) NOT NULL,
approved_by UUID NULL REFERENCES users (identifier),
created_at TIMESTAMPTZ NOT NULL,
last_edited_at TIMESTAMPTZ NULL,
UNIQUE (identifier),
PRIMARY KEY (identifier)
);

CREATE TABLE genre_external_sources(
identifier UUID NOT NULL,
genre_identifier UUID NOT NULL REFERENCES genres (identifier),
created_by UUID NOT NULL REFERENCES users (identifier),
spotify_url VARCHAR NULL,
youtube_url VARCHAR NULL,
soundcloud_url VARCHAR NULL,
wikipedia_url VARCHAR NULL,
created_at TIMESTAMPTZ NOT NULL,
last_edited_at TIMESTAMPTZ NULL,
UNIQUE(genre_identifier),
UNIQUE (identifier),
PRIMARY KEY (identifier)
);

CREATE TABLE genre_opinions(
identifier UUID NOT NULL,
genre_identifier UUID NOT NULL REFERENCES genres (identifier),
created_by UUID NOT NULL REFERENCES users (identifier),
is_like BOOLEAN NOT NULL,
is_dislike BOOLEAN NOT NULL,
created_at TIMESTAMPTZ NOT NULL,
last_edited_at TIMESTAMPTZ NULL,
UNIQUE(created_by, genre_identifier),
UNIQUE (identifier),
PRIMARY KEY (identifier)
);

CREATE TABLE user_favourite_genres(
identifier UUID NOT NULL,
user_identifier UUID NOT NULL REFERENCES users (identifier),
genre_identifier UUID NOT NULL REFERENCES genres (identifier),
created_at TIMESTAMPTZ NOT NULL,
last_edited_at TIMESTAMPTZ NULL,
UNIQUE(genre_identifier, user_identifier),
UNIQUE (identifier),
PRIMARY KEY (identifier)
);

CREATE TABLE song_artworks(
identifier UUID NOT NULL,
song_identifier UUID NOT NULL REFERENCES songs (identifier),
created_by UUID NOT NULL REFERENCES users (identifier),
visibility_status INT NOT NULL,
approved_by UUID NULL REFERENCES users (identifier),
content_url VARCHAR NOT NULL,
content_caption VARCHAR(8200) NULL,
order_value INT NOT NULL,
created_at TIMESTAMPTZ NOT NULL,
last_edited_at TIMESTAMPTZ NULL,
UNIQUE (identifier),
PRIMARY KEY (identifier)
);

CREATE TABLE song_comments(
identifier UUID NOT NULL,
parent_identifier UUID NULL REFERENCES song_comments (identifier),
song_identifier UUID NOT NULL REFERENCES songs (identifier),
created_by UUID NOT NULL REFERENCES users (identifier),
visibility_status INT NOT NULL,
contents VARCHAR(8200) NOT NULL,
approved_by UUID NULL REFERENCES users (identifier),
created_at TIMESTAMPTZ NOT NULL,
last_edited_at TIMESTAMPTZ NULL,
UNIQUE (identifier),
PRIMARY KEY (identifier)
);

CREATE TABLE song_contents(
identifier UUID NOT NULL,
song_identifier UUID NOT NULL REFERENCES songs (identifier),
version_name VARCHAR(400) NOT NULL,
created_by UUID NOT NULL REFERENCES users (identifier),
visibility_status INT NOT NULL,
approved_by UUID NULL REFERENCES users (identifier),
instrument_type VARCHAR NOT NULL,
ascii_legend TEXT NULL,
ascii_contents TEXT NULL,
pdf_contents TEXT NULL,
guitarpro_contents TEXT NULL,
created_at TIMESTAMPTZ NOT NULL,
last_edited_at TIMESTAMPTZ NULL,
UNIQUE (song_identifier, version_name),
UNIQUE (identifier),
PRIMARY KEY (identifier)
);

CREATE TABLE song_external_sources(
identifier UUID NOT NULL,
song_identifier UUID NOT NULL REFERENCES songs (identifier),
created_by UUID NOT NULL REFERENCES users (identifier),
spotify_url VARCHAR NULL,
youtube_url VARCHAR NULL,
soundcloud_url VARCHAR NULL,
wikipedia_url VARCHAR NULL,
created_at TIMESTAMPTZ NOT NULL,
last_edited_at TIMESTAMPTZ NULL,
UNIQUE (song_identifier),
UNIQUE (identifier),
PRIMARY KEY (identifier)
);

CREATE TABLE song_opinions(
identifier UUID NOT NULL,
song_identifier UUID NOT NULL REFERENCES songs (identifier),
created_by UUID NOT NULL REFERENCES users (identifier),
is_like BOOLEAN NOT NULL,
is_dislike BOOLEAN NOT NULL,
created_at TIMESTAMPTZ NOT NULL,
last_edited_at TIMESTAMPTZ NULL,
UNIQUE(created_by, song_identifier),
UNIQUE (identifier),
PRIMARY KEY (identifier)
);

CREATE TABLE user_favourite_songs(
identifier UUID NOT NULL,
user_identifier UUID NOT NULL REFERENCES users (identifier),
song_folder_identifier UUID NOT NULL REFERENCES user_favourite_song_folders (identifier),
song_identifier UUID NOT NULL REFERENCES songs (identifier),
created_at TIMESTAMPTZ NOT NULL,
UNIQUE(user_identifier, song_folder_identifier),
UNIQUE(song_folder_identifier, song_identifier),
UNIQUE (identifier),
PRIMARY KEY (identifier)
);

CREATE TABLE user_donations(
identifier UUID NOT NULL,
user_identifier UUID NOT NULL REFERENCES users (identifier),
currency VARCHAR NOT NULL,
donated_amount NUMERIC NOT NULL CHECK (donated_amount > 0),
visibility_status INT NOT NULL,
created_at TIMESTAMPTZ NOT NULL,
last_edited_at TIMESTAMPTZ NULL,
UNIQUE (identifier),
PRIMARY KEY (identifier)
);

CREATE TABLE user_roles(
identifier UUID NOT NULL,
user_identifier UUID NOT NULL REFERENCES users (identifier),
role_id INT NOT NULL,
created_at TIMESTAMPTZ NOT NULL,
UNIQUE (user_identifier),
UNIQUE (identifier),
PRIMARY KEY (identifier)
);

CREATE TABLE user_trust_scores(
identifier UUID NOT NULL,
user_identifier UUID NOT NULL REFERENCES users (identifier),
happy_events INT NULL,
unhappy_events INT NULL,
superuser_trust_multiplier FLOAT NULL,
total_trust FLOAT NULL,
created_at TIMESTAMPTZ NOT NULL,
updated_at TIMESTAMPTZ NULL,
UNIQUE (user_identifier),
UNIQUE (identifier),
PRIMARY KEY (identifier)
);

CREATE TABLE song_genres(
identifier UUID NOT NULL,
song_identifier UUID NOT NULL REFERENCES songs (identifier),
genre_identifier UUID NOT NULL REFERENCES genres (identifier),
created_by UUID NOT NULL REFERENCES users (identifier),
created_at TIMESTAMPTZ NOT NULL,
UNIQUE(song_identifier, genre_identifier),
UNIQUE (identifier),
PRIMARY KEY (identifier)
);

CREATE TABLE song_artists(
identifier UUID NOT NULL,
song_identifier UUID NOT NULL REFERENCES songs (identifier),
artist_identifier UUID NOT NULL REFERENCES artists (identifier),
created_by UUID NOT NULL REFERENCES users (identifier),
created_at TIMESTAMPTZ NOT NULL,
UNIQUE(song_identifier, artist_identifier),
UNIQUE (identifier),
PRIMARY KEY (identifier)
);

CREATE TABLE artist_genres(
identifier UUID NOT NULL,
artist_identifier UUID NOT NULL REFERENCES artists (identifier),
genre_identifier UUID NOT NULL REFERENCES genres (identifier),
created_by UUID NOT NULL REFERENCES users (identifier),
created_at TIMESTAMPTZ NOT NULL,
UNIQUE(artist_identifier, genre_identifier),
UNIQUE (identifier),
PRIMARY KEY (identifier)
);

CREATE TABLE forum_page_forum_categories(
identifier UUID NOT NULL,
forum_page_identifier UUID NOT NULL REFERENCES forum_pages (identifier),
forum_category_identifier UUID NOT NULL REFERENCES forum_categories (identifier),
created_by UUID NOT NULL REFERENCES users (identifier),
created_at TIMESTAMPTZ NOT NULL,
UNIQUE(forum_page_identifier, forum_category_identifier),
UNIQUE (identifier),
PRIMARY KEY (identifier)
);

