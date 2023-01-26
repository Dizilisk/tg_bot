DROP TABLE baka_bot.pidor_winners;

CREATE TABLE baka_bot.pidor_winners_new (
    "date" TEXT,
    "user_id" BIGINT,
    "chat_id" BIGINT,
    PRIMARY KEY ("date", "chat_id"),
    FOREIGN KEY (user_id) REFERENCES baka_bot.users (user_id) ON DELETE CASCADE
);