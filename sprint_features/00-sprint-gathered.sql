SELECT ${f(join_cols, "sprint")},
	CAST(MIN(update_tracker.update_date) > ${s(sprint_close)} AS INT) AS sprint_is_complete
FROM gros.update_tracker
JOIN gros.${t("sprint")}
ON ${j(join_cols, "update_tracker", "sprint", mask=1)}
${s(component_join, project="sprint")}
${g(join_cols, "sprint", f("sprint_close"))}
