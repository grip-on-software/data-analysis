SELECT DISTINCT ${f(join_cols, "issue")}, ${s(issue_key)} AS key
FROM gros.${t("issue")}
JOIN gros.${t("sprint")} ON ${j(join_cols, "issue", "sprint")}
${s(issue_join)}
WHERE ${s(issue_story)}
AND ${t("issue")}.updated > ${s(sprint_open)}
AND ${s(sprint_id, sprint="issue")} <> 0
${s(project_condition, project="issue")}
