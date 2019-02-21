SELECT ${t("issue")}.issue_id, issue_changes.earliest_date,
issue_changes.status AS old_status, issue_changes.resolution AS old_resolution,
${s(issue_status)} AS new_status, ${s(issue_resolution)} AS new_resolution,
${t("issue")}.updated AS new_date
FROM gros.${t("issue")}
${s(issue_status_resolution_join)}
JOIN (
	SELECT issue_id,
		${s(issue_status)} AS status, ${s(issue_resolution)} AS resolution,
		MIN(updated) AS earliest_date, MAX(changelog_id) AS changelog_id
	FROM gros.${t("issue")}
	${s(issue_status_resolution_join)}
	GROUP BY issue_id, ${f("issue_status")}, ${f("issue_resolution")}
) AS issue_changes
ON ${j(issue_next_changelog, "issue", "issue_changes")}
WHERE ${s(issue_story_subtask)}
${category_conditions}
