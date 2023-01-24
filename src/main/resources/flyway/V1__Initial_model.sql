CREATE schema baka_bot;

CREATE TABLE baka_bot.users (
    "user_id" BIGINT PRIMARY KEY,
    "user_name" VARCHAR(50),
    "first_name" VARCHAR(50),
    "last_name" VARCHAR(50)
);

CREATE TABLE baka_bot.game_stat (
    "chat_id" BIGINT,
    "user_id" BIGINT,
    "win_count" INT DEFAULT (0),
    "lose_count" INT DEFAULT (0),
    FOREIGN KEY (user_id) REFERENCES baka_bot.users (user_id) ON DELETE CASCADE
);

CREATE TABLE baka_bot.pidor_stat (
    "chat_id" BIGINT,
    "user_id" BIGINT,
    "pidor_count" INT DEFAULT (0),
    FOREIGN KEY (user_id) REFERENCES baka_bot.users (user_id) ON DELETE CASCADE
);



