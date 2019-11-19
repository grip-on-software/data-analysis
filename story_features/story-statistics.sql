SELECT ${f(join_cols, "issue")}, ${s(story_points)} AS story_points,
	attachments, labels, watchers, expected_ltcs, expected_phtcs,
	LENGTH(description) AS description_length, priority, ready_status,
	NOT ${s(issue_closed}) AND CAST(duedate AS TIMESTAMP) <= ${current_timestamp} AS overdue
FROM gros.${t("issue")}
WHERE ${s(issue_story)}
${s(project_condition, project="issue")}
