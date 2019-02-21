SELECT ${f(join_cols, "commits")}, COUNT(*) AS num_commits
FROM gros.${t("commits")}
${g(join_cols, "commits")} HAVING ${f(join_cols, "commits")} IS NOT NULL
