-- Number of issues with distinct titles (to avoid automated subtasks)
SELECT ${f(join_cols, "issue")}, COUNT(DISTINCT title) AS num_issues
FROM gros.${t("issue")}
${g(join_cols, "issue")}
