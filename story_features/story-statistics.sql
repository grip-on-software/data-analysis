SELECT ${f(join_cols, "issue")}, ${s(story_points)} AS story_points,
	attachments, labels, watchers, expected_ltcs, expected_phtcs
FROM gros.${t("issue")}
WHERE ${s(issue_story)}
${s(project_condition, project="issue")}
