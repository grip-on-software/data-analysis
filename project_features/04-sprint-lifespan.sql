SELECT ${f(join_cols, "sprint")},
	EXTRACT(day FROM MAX(${s(sprint_close)})-MIN(start_date)) AS lifespan
FROM gros.${t("sprint")}
${g(join_cols, "sprint")}
