CREATE TYPE pronoun AS ENUM ('he', 'she', 'it', 'they', 'gendergap', 'unk');
CREATE TABLE "users"
(
	"telegram_id" integer NOT NULL,
	"username" text, 
	"pronoun" text NOT NULL DEFAULT 'unk',
	"used_mask" integer NOT NULL DEFAULT 0,
	"used_fem" integer NOT NULL DEFAULT 0,
	"used_plur" integer NOT NULL DEFAULT 0
);
