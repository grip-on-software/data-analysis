SELECT DISTINCT ${f(join_cols, "issue")}, ${s(issue_key)} AS key,
	${s(story_points)} AS story_points
FROM gros.${t("issue")}
JOIN gros.${t("sprint")} ON ${j(join_cols, "issue", "sprint")}
JOIN (SELECT issue_id, MAX(changelog_id) AS changelog_id
	FROM gros.${t("issue")} GROUP BY issue_id
) AS max_issue
ON ${j(issue_changelog, "issue", "max_issue")}
LEFT JOIN gros.subtask ON ${t("issue")}.issue_id = subtask.id_subtask
WHERE ${t("issue")}.updated > ${s(sprint_open)}
AND ${s(issue_not_done)} AND ${s(issue_overdue)}
AND ${t("issue")}.story_points IS NOT NULL
AND subtask.id_parent IS NULL
