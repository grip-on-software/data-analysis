SELECT ${f(join_cols, "sprint", alias=T, sprint="interval_sprint")}, ${s(issue_key)} AS key,
    MAX(${s(story_points)}) AS story_points,
    MAX(issue.fixversion) AS fixversion
FROM gros.${t("issue")}
LEFT JOIN gros.${t("issue")} AS newer_issue
ON ${j(issue_next_changelog, "newer_issue", "issue")}
LEFT JOIN gros.${t("sprint")}
ON ${j(join_cols, "issue", "sprint")}
JOIN gros.${t("sprint")} AS interval_sprint
ON ${j(join_cols, "issue", "interval_sprint", 1)}
${s(issue_join)}
WHERE (
    ${f(join_cols, "sprint", mask=2, alias="alias")} IS NULL
    OR ${s(issue_open)}
    OR ${s(sprint_close)} >= ${s(sprint_close, sprint="interval_sprint")}
)
AND ${s(issue_backlog)}
AND ${t("issue")}.updated <= ${s(sprint_close, sprint="interval_sprint")}
AND COALESCE(newer_issue.updated, ${s(sprint_close, sprint="interval_sprint")}) >= ${s(sprint_close, sprint="interval_sprint")}
${s(project_condition, project="issue")}
${g(join_cols, "sprint", f("issue_key"), sprint="interval_sprint")}
