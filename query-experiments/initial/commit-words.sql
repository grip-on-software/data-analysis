SELECT str_count(message, ' ') AS words, COUNT(*) FROM gros.commits GROUP BY words ORDER BY words;
