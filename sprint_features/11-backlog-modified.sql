SELECT ${f(join_cols, "sprint", alias=T, sprint="interval_sprint")}, ${s(issue_key)} AS key,
    SUM(COALESCE(${s(story_points)}, 0) - COALESCE(${s(story_points, issue="older_issue")}, 0)) AS story_points,
    MIN(${s(story_points, issue="older_issue")}) AS old_story_points,
    MAX(${s(story_points)}) AS new_story_points,
    MAX(${s(fix_version)}) AS fixversion
FROM gros.${t("issue")}
JOIN gros.${t("issue")} AS older_issue
ON ${j(issue_next_changelog, "issue", "older_issue")}
AND COALESCE(${t("issue")}.story_points, 0) <> COALESCE(older_issue.story_points, 0)
LEFT JOIN gros.${t("sprint")}
ON ${j(join_cols, "issue", "sprint")}
JOIN gros.${t("sprint")} AS interval_sprint
ON ${j(join_cols, "issue", "interval_sprint", 1)}
AND interval_sprint.sprint_id IN (${filter_sprint_ids})
AND ${t("issue")}.updated <= ${s(sprint_close, sprint="interval_sprint")}
AND ${t("issue")}.updated >= ${s(sprint_open, sprint="interval_sprint")}
${s(issue_join)}
WHERE (
    ${f(join_cols, "sprint", mask=2, alias="alias")} IS NULL
    OR ${s(issue_open)}
    OR ${s(sprint_close)} >= ${s(sprint_close, sprint="interval_sprint")}
)
AND ${s(issue_backlog)}
${s(project_condition, project="issue")}
${s(filter_condition)}
${g(join_cols, "sprint", f("issue_key"), sprint="interval_sprint")}
