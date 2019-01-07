SELECT ${f(join_cols, "issue")}, AVG(EXTRACT(day FROM ${t("issue")}.updated - ${s(sprint_open)})) AS avg_change_day
FROM gros.${t("issue")}, gros.${t("sprint")}
WHERE ${j(join_cols, "issue", "sprint")}
AND ${t("issue")}.updated < sprint.end_date
GROUP BY ${f(join_cols, "issue")}
