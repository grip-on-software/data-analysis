SELECT ${f(join_cols, "issue")}, AVG(max_issue.num_changes) AS avg_changes
FROM gros.issue, (
    SELECT ${t("issue")}.issue_id, MAX(${t("issue")}.changelog_id) AS changelog_id, COUNT(*) AS num_changes
    FROM gros.${t("issue")}
    GROUP BY ${t("issue")}.issue_id
) AS max_issue
WHERE ${j(issue_changelog, "issue", "max_issue")}
AND ${t("issue")}.sprint_id <> 0
GROUP BY ${f(join_cols, "issue")}
