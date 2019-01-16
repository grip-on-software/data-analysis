SELECT ${f(join_cols, "issue")}, AVG(max_issue.num_changes) AS avg_changes
FROM gros.issue
JOIN (
    SELECT ${t("issue")}.issue_id, MAX(${t("issue")}.changelog_id) AS changelog_id, COUNT(*) AS num_changes
    FROM gros.${t("issue")}
    GROUP BY ${t("issue")}.issue_id
) AS max_issue
ON ${j(issue_changelog, "issue", "max_issue")}
${s(issue_join)}
WHERE ${t("issue")}.sprint_id <> 0
${g(join_cols, "issue")}
