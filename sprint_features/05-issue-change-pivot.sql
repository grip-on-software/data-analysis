SELECT ${f(join_cols, "issue")}, AVG(EXTRACT(day FROM ${t("issue")}.updated - ${s(sprint_open)})) AS avg_change_day
FROM gros.${t("issue")}
JOIN gros.${t("sprint")}
ON ${j(join_cols, "issue", "sprint")}
${s(issue_join)}
WHERE ${t("issue")}.updated < ${s(sprint_close)}
${s(project_condition, project="issue")}
${g(join_cols, "issue")}
