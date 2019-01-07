-- Number of Jira updaters in sprint
SELECT ${f(join_cols, "sprint_devs")}, COUNT(*) AS number_of_jira_devs FROM
    (SELECT ${f(join_cols, "issue")}, issue.updated_by AS developer_id FROM gros.issue
    GROUP BY ${f(join_cols, "issue")}, developer_id HAVING issue.sprint_id <> 0
    UNION ALL
    SELECT ${f(join_cols, "issue")}, comment.author AS developer_id FROM gros.comment
    JOIN gros.${t("issue")} ON ${t("issue")}.issue_id = comment.issue_id
    GROUP BY ${f(join_cols, "issue")}, developer_id HAVING issue.sprint_id <> 0
    ) AS sprint_devs
GROUP BY ${f(join_cols, "sprint_devs")}
