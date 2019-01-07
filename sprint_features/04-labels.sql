SELECT ${f(join_cols, "issue")}, SUM(${t("issue")}.labels) AS num_labels
FROM gros.issue,
    (SELECT ${t("issue")}.issue_id, MAX(${t("issue")}.changelog_id) AS changelog_id
    FROM gros.${t("issue")} GROUP BY ${t("issue")}.issue_id) AS maxdata
WHERE ${j(issue_changelog, "issue", "maxdata")}
AND ${t("issue")}.sprint_id <> 0
GROUP BY ${f(join_cols, "issue")}
