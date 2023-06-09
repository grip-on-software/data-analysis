-- Number of done story points
SELECT DISTINCT ${f(join_cols, "issue")}, ${s(issue_key)} AS key,
    ${s(story_points, issue="resolve_issue")} AS story_points,
    ${s(fix_version, issue="resolve_issue")} AS fixversion
FROM gros.${t("issue")}
JOIN gros.${t("sprint")} ON ${j(join_cols, "issue", "sprint")}
JOIN (SELECT issue_id, ${f("issue_done")}, MAX(changelog_id) AS changelog_id
    FROM gros.${t("issue")} GROUP BY issue_id, ${f("issue_done")}
) AS max_issue ON ${t("issue")}.issue_id = max_issue.issue_id
JOIN gros.${t("issue")} AS resolve_issue
ON ${j(issue_changelog, "resolve_issue", "max_issue")}
LEFT JOIN gros.${t("issue")} AS later_issue
ON ${j(issue_next_changelog, "later_issue", "resolve_issue")}
LEFT JOIN gros.subtask ON resolve_issue.issue_id = subtask.id_subtask
${s(issue_join)}
WHERE ${s(issue_done)}
AND ${s(issue_done, issue="resolve_issue")}
AND ${j(join_cols, "issue", "resolve_issue", mask=2)}
AND (${s(issue_closed)} OR NOT (${s(issue_closed, issue="resolve_issue")}))
AND (later_issue.issue_id IS NULL OR later_issue.updated >= ${s(sprint_close)})
--AND NOT ${s(issue_overdue)}
AND ${t("issue")}.updated > ${s(sprint_open)}
AND ${s(story_point_types)}
AND ${t("issue")}.story_points IS NOT NULL
AND ${s(sprint_id, sprint="issue")} <> 0
AND subtask.id_parent IS NULL
${s(project_condition, project="issue")}
${s(filter_condition)}
