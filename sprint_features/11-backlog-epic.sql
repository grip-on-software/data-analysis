SELECT ${f(join_cols, "sprint", alias=T, sprint="interval_sprint")}, ${t("issue")}.epic AS key,
    COUNT(*) AS epic_children,
    SUM(${s(story_points)}) AS story_points
FROM gros.${t("issue")}
LEFT JOIN gros.${t("issue")} AS newer_issue
ON ${j(issue_next_changelog, "newer_issue", "issue")}
LEFT JOIN gros.${t("sprint")} ON ${j(join_cols, "issue", "sprint")}
JOIN gros.${t("sprint")} AS interval_sprint
ON ${j(join_cols, "issue", "interval_sprint", 1)}
${s(issue_join)}
WHERE ${t("issue")}.epic IS NOT NULL
AND (${f(join_cols, "sprint", mask=2, alias="alias")} = NULL
	OR ${s(sprint_open)} >= ${s(sprint_close, sprint="interval_sprint")})
AND ${s(issue_story)}
AND ${s(issue_not_done)}
AND ${t("issue")}.updated <= ${s(sprint_close, sprint="interval_sprint")}
AND (
        newer_issue.updated IS NULL OR
        newer_issue.updated > ${s(sprint_close, sprint="interval_sprint")}
)
${s(project_condition, project="issue")}
${g(join_cols, "sprint", sprint="interval_sprint")}, ${t("issue")}.epic
