-- Completed story points
SELECT ${f(join_cols, "start_issue")},
	-(${s(story_points, issue="start_issue")}) AS story_points,
	${s(issue_key, issue="start_issue")} AS key,
	MIN(${t("issue")}.updated) AS close_date, 'completed' AS event_type
FROM gros.${t("issue")} AS start_issue
JOIN gros.${t("issue")} ON start_issue.issue_id = ${t("issue")}.issue_id
	AND start_issue.changelog_id < ${t("issue")}.changelog_id
	AND ${s(issue_done)}
JOIN gros.${t("sprint")} ON ${j(join_cols, "start_issue", "sprint")}
WHERE ${s(issue_story)} AND ${s(issue_done)}
	AND start_issue.updated > ${s(sprint_open)}
	AND start_issue.story_points IS NOT NULL 
${g(join_cols, "start_issue")}, ${f("issue_key", issue="start_issue")},
	${f("story_points", issue="start_issue")}
-- Stories added during the sprint
UNION ALL
SELECT ${f(join_cols, "late_issue")},
	${s(story_points, issue="late_issue")} AS story_points,
	${s(issue_key, issue="late_issue")} AS key,
	late_issue.updated AS close_date,
	'scope_add' AS event_type
FROM gros.${t("issue")} AS late_issue
LEFT JOIN gros.${t("issue")}
	ON ${j(issue_next_changelog, "late_issue", "issue")}
JOIN gros.${t("sprint")} ON ${j(join_cols, "late_issue", "sprint")}
WHERE COALESCE(${t("issue")}.sprint_id, 0) <> late_issue.sprint_id
	AND (late_issue.changelog_id = 0 OR ${t("issue")}.changelog_id IS NOT NULL)
	AND late_issue.updated BETWEEN ${t("sprint")}.start_date AND ${t("sprint")}.end_date
	AND ${s(issue_story, issue="late_issue")}
-- Stories removed during the sprint
UNION ALL
SELECT ${f(join_cols, "remove_issue")},
	-(${s(story_points, issue="remove_issue")}) AS story_points,
	${s(issue_key, issue="remove_issue")} AS key,
	remove_issue.updated AS close_date, 'scope_remove' AS event_type
FROM gros.${t("issue")} AS remove_issue
JOIN gros.${t("issue")} ON ${j(issue_next_changelog, "remove_issue", "issue")}
JOIN gros.${t("sprint")} ON ${j(join_cols, "issue", "sprint")}
WHERE ${t("issue")}.sprint_id <> remove_issue.sprint_id
	AND remove_issue.updated BETWEEN ${t("sprint")}.start_date AND ${t("sprint")}.end_date
	AND ${s(issue_story, issue="remove_issue")}
-- Story points change during the sprint
UNION ALL
SELECT ${f(join_cols, "edit_issue")},
	${s(story_points, issue="edit_issue")} - COALESCE(${s(story_points)}, 0) AS story_points,
	${s(issue_key, issue="edit_issue")} AS key,
	edit_issue.updated AS close_date, 'points' AS event_type
FROM gros.${t("issue")} AS edit_issue
LEFT JOIN gros.${t("issue")}
	ON ${j(issue_next_changelog, "edit_issue", "issue")}
JOIN gros.${t("sprint")} ON ${j(join_cols, "edit_issue", "sprint")}
WHERE ${t("issue")}.sprint_id = edit_issue.sprint_id
	AND COALESCE(${s(story_points)}, 0) <> ${s(story_points, issue="edit_issue")}
	AND edit_issue.updated BETWEEN ${t("sprint")}.start_date AND ${t("sprint")}.end_date
	AND ${s(issue_story, issue="edit_issue")}
-- Initial story points
UNION ALL
SELECT ${f(join_cols, "sprint")},
	COALESCE(SUM(${s(story_points, issue="init_issue")}), 0) AS story_points,
	null AS key,
	${t("sprint")}.start_date AS close_date, 'initial' AS event_type
FROM gros.${t("sprint")}
LEFT JOIN (
	SELECT ${f(join_cols, "issue")}, issue_id, story_points,
		MIN(updated) AS updated
	FROM gros.${t("issue")}
	WHERE ${s(issue_story)} AND ${t("issue")}.sprint_id IS NOT NULL
	${g(join_cols, "issue")}, issue_id, story_points
) AS init_issue
ON ${j(join_cols, "sprint", "init_issue")}
	AND init_issue.updated < ${t("sprint")}.start_date
${g(join_cols, "sprint")}, ${t("sprint")}.start_date
-- Close date
UNION ALL
SELECT ${f(join_cols, "sprint")}, null AS story_points, null AS key,
	${s(sprint_close)} AS close_date, 'close' AS event_type
FROM gros.${t("sprint")}
ORDER BY ${f(join_cols, "")}, close_date
