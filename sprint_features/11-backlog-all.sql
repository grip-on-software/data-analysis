SELECT ${f(join_cols, "interval_sprint", alias=T)}, ${s(issue_key)} AS key,
	MAX(${s(story_points)}) AS story_points
FROM gros.${t("issue")}
LEFT JOIN gros.${t("issue")} AS newer_issue
ON ${j(issue_next_changelog, "newer_issue", "issue")}
LEFT JOIN gros.${t("sprint")}
ON ${j(join_cols, "issue", "sprint")}
JOIN gros.${t("sprint")} AS interval_sprint
ON ${j(join_cols, "issue", "interval_sprint", 1)}
${s(issue_join)}
WHERE (${t("sprint")}.sprint_id = NULL
	OR ${s(sprint_open)} >= interval_sprint.start_date)
AND ${s(issue_not_done)}
AND ${s(issue_backlog)}
AND ${t("issue")}.updated <= interval_sprint.start_date
AND (
	newer_issue.updated IS NULL OR
	newer_issue.updated > interval_sprint.start_date
)
${g(join_cols, "interval_sprint", f("issue_key"))}
