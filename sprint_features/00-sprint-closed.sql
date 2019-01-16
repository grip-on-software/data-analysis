SELECT ${f(join_cols, "sprint")},
	CAST(${s(sprint_closed)} AS INT) AS sprint_is_closed
FROM gros.${t("sprint")}
${s(component_join, project="sprint")}
