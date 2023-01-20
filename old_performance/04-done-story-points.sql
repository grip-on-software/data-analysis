-- Number of done story points
SELECT ${f(join_cols, "done_stories")}, ${s(issue_key)} AS key,
    ${s(story_points)} AS story_points,
    ${s(fix_version, issue="done_stories")} AS fixversion
FROM
	gros.${t("issue")},
	(SELECT DISTINCT ${f(join_cols, "issue")}, ${t("issue")}.issue_id, max_issue.changelog_id, ${s(fix_version, issue="resolve_issue")} AS fixversion
		FROM gros.${t("issue")}
		JOIN gros.${t("sprint")} ON ${j(join_cols, "issue", "sprint")}
		JOIN (SELECT issue_id, sprint_id, MAX(changelog_id) AS changelog_id
			FROM gros.${t("issue")} GROUP BY issue_id, sprint_id
		) AS max_issue ON ${t("issue")}.issue_id = max_issue.issue_id AND ${t("issue")}.sprint_id = max_issue.sprint_id
		JOIN gros.${t("issue")} AS resolve_issue ON ${j(issue_changelog, "resolve_issue", "max_issue")}
    	LEFT JOIN gros.subtask ON resolve_issue.issue_id = subtask.id_subtask
        ${s(issue_join)}
		WHERE ${s(issue_done)}
		AND ${s(issue_done, issue="resolve_issue")}
		--AND NOT ${s(issue_overdue)}
		AND ${t("issue")}.updated > ${s(sprint_open)}
		AND ${s(story_point_types)}
		AND ${t("issue")}.story_points IS NOT NULL
		AND ${s(sprint_id, sprint="issue")} <> 0
    	AND subtask.id_parent IS NULL
        ${s(project_condition, project="issue")}
        ${s(filter_condition)}
	) AS done_stories
WHERE ${j(issue_changelog, "issue", "done_stories")}
