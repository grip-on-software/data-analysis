SELECT ${f(join_cols, "max_issue")}, COUNT(*) AS num_early_storypoint_changes
FROM gros.${t("issue")}
LEFT JOIN gros.${t("issue")} AS prev_issue
ON ${j(issue_next_changelog, "issue", "prev_issue")}, (
	SELECT ${f(join_cols, "issue")}, ${t("issue")}.issue_id,
		MAX(${t("issue")}.changelog_id) AS changelog_id
        FROM gros.${t("issue")}
        GROUP BY ${f(join_cols, "issue")}, ${t("issue")}.issue_id
    ) AS max_issue,
    gros.${t("sprint")}
WHERE ${t("issue")}.issue_id = max_issue.issue_id
AND ${t("issue")}.changelog_id <= max_issue.changelog_id
AND ${j(join_cols, "max_issue", "sprint")}
AND ${t("issue")}.story_points IS NOT NULL
AND (${t("issue")}.changelog_id = 0
	OR ${t("issue")}.story_points <> prev_issue.story_points)
AND ${t("issue")}.updated < ${s(planned_early)}
GROUP BY ${f(join_cols, "max_issue")}
