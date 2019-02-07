SELECT ${f(join_cols, "commits")}, COUNT(*) AS num_merges
FROM gros.${t("commits")}
WHERE type = 'merge'
${g(join_cols, "commits")}
