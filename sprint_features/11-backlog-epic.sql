SELECT ${f(join_cols, "interval_sprint")}, ${t("issue")}.epic AS key,
    COUNT(*) AS epic_children,
    SUM(${s(story_points)}) AS story_points
FROM gros.${t("issue")}
LEFT JOIN gros.${t("issue")} AS newer_issue
ON ${j(issue_next_changelog, "newer_issue", "issue")}
LEFT JOIN gros.${t("sprint")} ON ${j(join_cols, "issue", "sprint")},
gros.sprint AS interval_sprint
WHERE ${j(join_cols[1], "issue", "interval_sprint")}
AND ${t("issue")}.epic IS NOT NULL
AND (${t("sprint")}.sprint_id = NULL
	OR ${s(sprint_open)} >= interval_sprint.start_date)
AND ${s(issue_story)}
AND ${s(issue_not_done)}
AND ${t("issue")}.updated <= interval_sprint.start_date
AND (
        newer_issue.updated IS NULL OR
        newer_issue.updated > interval_sprint.start_date
)
GROUP BY ${f(join_cols, "interval_sprint")}, ${t("issue")}.epic
