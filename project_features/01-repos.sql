SELECT ${f(join_cols, "repo")}, COUNT(*) AS num_repos
FROM gros.${t("repo")}
${g(join_cols, "repo")} HAVING ${f(join_cols, "repo")} IS NOT NULL
