SELECT ${f(join_cols, "issue")}, COUNT(*) AS num_late_rank_changes
FROM gros.${t("issue")}
JOIN gros.${t("sprint")} ON ${t("issue")}.sprint_id = ${t("sprint")}.sprint_id
WHERE ${t("issue")}.sprint_id <> 0 AND ${t("issue")}.rank_change IS NOT NULL
AND ${t("issue")}.updated > ${s(planned_end)}
GROUP BY ${f(join_cols, "issue")}
