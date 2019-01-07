SELECT ${f(join_cols, "max_issue")}, COUNT(*) AS num_early_changes
FROM gros.${t("issue")},
    (SELECT ${f(join_cols, "issue")}, ${t("issue")}.issue_id,
		MAX(${t("issue")}.changelog_id) AS changelog_id
        FROM gros.${t("issue")}
        GROUP BY ${f(join_cols, "issue")}, ${t("issue")}.issue_id
    ) AS max_issue,
    gros.${t("sprint")}
WHERE ${t("issue")}.issue_id = max_issue.issue_id
AND ${t("issue")}.changelog_id <= max_issue.changelog_id
AND ${j(join_cols, "max_issue", "sprint")}
AND ${t("issue")}.rank_change IS NULL
AND ${t("issue")}.updated < ${s(planned_early)}
GROUP BY ${f(join_cols, "max_issue")}
