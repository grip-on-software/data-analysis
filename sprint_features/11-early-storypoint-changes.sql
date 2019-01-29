SELECT ${f(join_cols, "max_issue")}, COUNT(*) AS num_early_storypoint_changes
FROM gros.${t("issue")}
LEFT JOIN gros.${t("issue")} AS prev_issue
ON ${j(issue_next_changelog, "issue", "prev_issue")}
JOIN (
SELECT ${f(join_cols, "issue")}, ${t("issue")}.issue_id,
	MAX(${t("issue")}.changelog_id) AS changelog_id
    FROM gros.${t("issue")}
	${s(issue_join)}
    ${g(join_cols, "issue")}, ${t("issue")}.issue_id
) AS max_issue
ON ${t("issue")}.issue_id = max_issue.issue_id
AND ${t("issue")}.changelog_id <= max_issue.changelog_id
${s(issue_join)}
JOIN gros.${t("sprint")}
ON ${j(join_cols, "max_issue", "sprint")}
AND ${t("issue")}.story_points IS NOT NULL
AND (${t("issue")}.changelog_id = 0
	OR ${t("issue")}.story_points <> prev_issue.story_points)
AND ${t("issue")}.updated < ${s(planned_early)}
${s(project_condition, project="issue")}
${g(join_cols, "max_issue")}
