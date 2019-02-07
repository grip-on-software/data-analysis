SELECT ${f(join_cols, "sprint")}, COUNT(*) AS num_sprints
FROM gros.${t("sprint")}
${g(join_cols, "sprint")}
