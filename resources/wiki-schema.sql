CREATE TABLE artist_artworks (
  identifier TEXT NOT NULL PRIMARY KEY, 
  artist_identifier TEXT NOT NULL, 
  created_by TEXT NOT NULL, 
  visibility_status INTEGER NOT NULL, 
  approved_by TEXT, 
  content_url TEXT NOT NULL, 
  content_caption TEXT, 
  order_value INTEGER NOT NULL, 
  created_at TEXT NOT NULL, 
  last_edited_at TEXT, 
  FOREIGN KEY (artist_identifier) REFERENCES artists (identifier), 
  FOREIGN KEY (created_by) REFERENCES users (identifier), 
  FOREIGN KEY (approved_by) REFERENCES users (identifier)
);
CREATE TABLE artist_comments (
  identifier TEXT NOT NULL PRIMARY KEY, 
  parent_identifier TEXT, 
  artist_identifier TEXT NOT NULL, 
  created_by TEXT NOT NULL, 
  visibility_status INTEGER NOT NULL, 
  contents TEXT NOT NULL, 
  approved_by TEXT, 
  created_at TEXT NOT NULL, 
  last_edited_at TEXT, 
  FOREIGN KEY (artist_identifier) REFERENCES artists (identifier), 
  FOREIGN KEY (created_by) REFERENCES users (identifier), 
  FOREIGN KEY (approved_by) REFERENCES users (identifier)
);
CREATE TABLE artist_external_sources (
  identifier TEXT NOT NULL PRIMARY KEY, 
  artist_identifier TEXT NOT NULL, 
  created_by TEXT NOT NULL, 
  spotify_url TEXT, 
  youtube_url TEXT, 
  soundcloud_url TEXT, 
  wikipedia_url TEXT, 
  created_at TEXT NOT NULL, 
  last_edited_at TEXT, 
  FOREIGN KEY (artist_identifier) REFERENCES artists (identifier), 
  FOREIGN KEY (created_by) REFERENCES users (identifier)
);
CREATE TABLE artist_genres (
  identifier TEXT NOT NULL PRIMARY KEY, 
  artist_identifier TEXT NOT NULL, 
  genre_identifier TEXT NOT NULL, 
  created_by TEXT NOT NULL, 
  created_at TEXT NOT NULL, 
  FOREIGN KEY (artist_identifier) REFERENCES artists (identifier), 
  FOREIGN KEY (genre_identifier) REFERENCES genres (identifier)
);
CREATE TABLE artist_opinions (
  identifier TEXT NOT NULL PRIMARY KEY, 
  artist_identifier TEXT NOT NULL, 
  created_by TEXT NOT NULL, 
  is_like INTEGER NOT NULL, 
  is_dislike INTEGER NOT NULL, 
  created_at TEXT NOT NULL, 
  last_edited_at TEXT, 
  FOREIGN KEY (artist_identifier) REFERENCES artists (identifier), 
  FOREIGN KEY (created_by) REFERENCES users (identifier)
);
CREATE TABLE artists (
  identifier TEXT NOT NULL PRIMARY KEY, 
  display_name TEXT NOT NULL, 
  created_by TEXT NOT NULL, 
  visibility_status INTEGER NOT NULL, 
  approved_by TEXT, 
  created_at TEXT NOT NULL, 
  last_edited_at TEXT, 
  views INTEGER DEFAULT 0 NOT NULL, 
  description TEXT, 
  FOREIGN KEY (created_by) REFERENCES users (identifier), 
  FOREIGN KEY (approved_by) REFERENCES users (identifier)
);
CREATE TABLE genre_artworks (
  identifier TEXT NOT NULL PRIMARY KEY, 
  genre_identifier TEXT NOT NULL, 
  created_by TEXT NOT NULL, 
  visibility_status INTEGER NOT NULL, 
  approved_by TEXT, 
  content_url TEXT NOT NULL, 
  content_caption TEXT, 
  order_value INTEGER NOT NULL, 
  created_at TEXT NOT NULL, 
  last_edited_at TEXT, 
  FOREIGN KEY (created_by) REFERENCES users (identifier), 
  FOREIGN KEY (approved_by) REFERENCES users (identifier), 
  FOREIGN KEY (genre_identifier) REFERENCES genres (identifier)
);
CREATE TABLE genre_comments (
  identifier TEXT NOT NULL PRIMARY KEY, 
  parent_identifier TEXT, 
  genre_identifier TEXT NOT NULL, 
  created_by TEXT NOT NULL, 
  visibility_status INTEGER NOT NULL, 
  contents TEXT NOT NULL, 
  approved_by TEXT, 
  created_at TEXT NOT NULL, 
  last_edited_at TEXT, 
  FOREIGN KEY (created_by) REFERENCES users (identifier), 
  FOREIGN KEY (approved_by) REFERENCES users (identifier), 
  FOREIGN KEY (genre_identifier) REFERENCES genres (identifier)
);
CREATE TABLE genre_external_sources (
  identifier TEXT NOT NULL PRIMARY KEY, 
  genre_identifier TEXT NOT NULL, 
  created_by TEXT NOT NULL, 
  spotify_url TEXT, 
  youtube_url TEXT, 
  soundcloud_url TEXT, 
  wikipedia_url TEXT, 
  created_at TEXT NOT NULL, 
  last_edited_at TEXT, 
  FOREIGN KEY (created_by) REFERENCES users (identifier), 
  FOREIGN KEY (genre_identifier) REFERENCES genres (identifier)
);
CREATE TABLE genre_opinions (
  identifier TEXT NOT NULL PRIMARY KEY, 
  genre_identifier TEXT NOT NULL, 
  created_by TEXT NOT NULL, 
  is_like INTEGER NOT NULL, 
  is_dislike INTEGER NOT NULL, 
  created_at TEXT NOT NULL, 
  last_edited_at TEXT, 
  FOREIGN KEY (created_by) REFERENCES users (identifier), 
  FOREIGN KEY (genre_identifier) REFERENCES genres (identifier)
);
CREATE TABLE genres (
  identifier TEXT NOT NULL PRIMARY KEY, 
  parent_identifier TEXT, 
  display_name TEXT NOT NULL, 
  created_by TEXT NOT NULL, 
  visibility_status INTEGER NOT NULL, 
  approved_by TEXT, 
  created_at TEXT NOT NULL, 
  last_edited_at TEXT, 
  views INTEGER DEFAULT 0 NOT NULL, 
  description TEXT, 
  FOREIGN KEY (created_by) REFERENCES users (identifier), 
  FOREIGN KEY (approved_by) REFERENCES users (identifier)
);
CREATE TABLE song_artists (
  identifier TEXT NOT NULL PRIMARY KEY, 
  song_identifier TEXT NOT NULL, 
  artist_identifier TEXT NOT NULL, 
  created_by TEXT NOT NULL, 
  created_at TEXT NOT NULL, 
  FOREIGN KEY (created_by) REFERENCES users (identifier), 
  FOREIGN KEY (artist_identifier) REFERENCES artists (identifier), 
  FOREIGN KEY (song_identifier) REFERENCES songs (identifier)
);
CREATE TABLE song_artworks (
  identifier TEXT NOT NULL PRIMARY KEY, 
  song_identifier TEXT NOT NULL, 
  created_by TEXT NOT NULL, 
  visibility_status INTEGER NOT NULL, 
  approved_by TEXT, 
  content_url TEXT NOT NULL, 
  content_caption TEXT, 
  order_value INTEGER NOT NULL, 
  created_at TEXT NOT NULL, 
  last_edited_at TEXT, 
  FOREIGN KEY (created_by) REFERENCES users (identifier), 
  FOREIGN KEY (approved_by) REFERENCES users (identifier), 
  FOREIGN KEY (song_identifier) REFERENCES songs (identifier)
);
CREATE TABLE song_comments (
  identifier TEXT NOT NULL PRIMARY KEY, 
  parent_identifier TEXT, 
  song_identifier TEXT NOT NULL, 
  created_by TEXT NOT NULL, 
  visibility_status INTEGER NOT NULL, 
  contents TEXT NOT NULL, 
  approved_by TEXT, 
  created_at TEXT NOT NULL, 
  last_edited_at TEXT, 
  FOREIGN KEY (created_by) REFERENCES users (identifier), 
  FOREIGN KEY (approved_by) REFERENCES users (identifier), 
  FOREIGN KEY (song_identifier) REFERENCES songs (identifier)
);
CREATE TABLE song_contents (
  identifier TEXT NOT NULL PRIMARY KEY, 
  song_identifier TEXT NOT NULL, 
  version_name TEXT NOT NULL, 
  created_by TEXT NOT NULL, 
  visibility_status INTEGER NOT NULL, 
  approved_by TEXT, 
  instrument_type TEXT NOT NULL, 
  ascii_legend text, 
  ascii_contents text, 
  pdf_contents text, 
  guitarpro_contents text, 
  created_at TEXT NOT NULL, 
  last_edited_at TEXT, 
  FOREIGN KEY (created_by) REFERENCES users (identifier), 
  FOREIGN KEY (approved_by) REFERENCES users (identifier), 
  FOREIGN KEY (song_identifier) REFERENCES songs (identifier)
);
CREATE TABLE song_external_sources (
  identifier TEXT NOT NULL PRIMARY KEY, 
  song_identifier TEXT NOT NULL, 
  created_by TEXT NOT NULL, 
  spotify_url TEXT, 
  youtube_url TEXT, 
  soundcloud_url TEXT, 
  wikipedia_url TEXT, 
  created_at TEXT NOT NULL, 
  last_edited_at TEXT, 
  FOREIGN KEY (created_by) REFERENCES users (identifier), 
  FOREIGN KEY (song_identifier) REFERENCES songs (identifier)
);
CREATE TABLE song_genres (
  identifier TEXT NOT NULL PRIMARY KEY, 
  song_identifier TEXT NOT NULL, 
  genre_identifier TEXT NOT NULL, 
  created_by TEXT NOT NULL, 
  created_at TEXT NOT NULL, 
  FOREIGN KEY (created_by) REFERENCES users (identifier), 
  FOREIGN KEY (genre_identifier) REFERENCES genres (identifier), 
  FOREIGN KEY (song_identifier) REFERENCES songs (identifier)
);
CREATE TABLE song_opinions (
  identifier TEXT NOT NULL PRIMARY KEY, 
  song_identifier TEXT NOT NULL, 
  created_by TEXT NOT NULL, 
  is_like INTEGER NOT NULL, 
  is_dislike INTEGER NOT NULL, 
  created_at TEXT NOT NULL, 
  last_edited_at TEXT, 
  FOREIGN KEY (created_by) REFERENCES users (identifier), 
  FOREIGN KEY (song_identifier) REFERENCES songs (identifier)
);
CREATE TABLE songs (
  identifier TEXT NOT NULL PRIMARY KEY, 
  display_name TEXT NOT NULL, 
  music_key TEXT, 
  music_tuning TEXT, 
  music_creation_date TEXT, 
  album_name TEXT, 
  album_info_link TEXT, 
  created_by TEXT NOT NULL, 
  visibility_status INTEGER NOT NULL, 
  approved_by TEXT, 
  created_at TEXT NOT NULL, 
  last_edited_at TEXT, 
  views INTEGER DEFAULT 0 NOT NULL, 
  description TEXT, 
  FOREIGN KEY (created_by) REFERENCES users (identifier), 
  FOREIGN KEY (approved_by) REFERENCES users (identifier)
);
CREATE TABLE user_roles (
  identifier TEXT NOT NULL PRIMARY KEY, 
  user_identifier TEXT NOT NULL, 
  role_id TEXT NOT NULL, 
  created_at TEXT NOT NULL, 
  FOREIGN KEY (user_identifier) REFERENCES users (identifier)
  );
CREATE TABLE users (
  identifier TEXT NOT NULL PRIMARY KEY, 
  display_name TEXT NOT NULL, email_address TEXT NOT NULL, 
  password_hash TEXT NOT NULL, password_reset_token TEXT, 
  latest_login_at TEXT, latest_login_device TEXT, 
  avatar_url TEXT, created_at TEXT NOT NULL, 
  last_edited_at TEXT, auth_token TEXT, 
  description TEXT
);
