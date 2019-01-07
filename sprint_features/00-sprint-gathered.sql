SELECT ${f(join_cols, "sprint")}, CAST(MIN(update_tracker.update_date) > ${s(sprint_close)} AS INT) AS sprint_is_complete
FROM gros.update_tracker
JOIN gros.${t("sprint")} ON update_tracker.project_id = ${t("sprint")}.project_id
GROUP BY ${f(join_cols, "sprint")}, ${f("sprint_close")}
