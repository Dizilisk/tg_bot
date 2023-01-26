CREATE TABLE baka_bot.pidor_winners (
    "date" TEXT PRIMARY KEY,
    "user_id" BIGINT,
    "chat_id" BIGINT,
    FOREIGN KEY (user_id) REFERENCES baka_bot.users (user_id) ON DELETE CASCADE
);

DROP TABLE baka_bot.aaaa;