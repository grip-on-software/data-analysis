SELECT ${f(join_cols, "issue_updaters")}, AVG(issue_updaters.num_updaters) AS avg_updaters
FROM
    (SELECT ${f(join_cols, "issue")}, ${t("issue")}.issue_id, COUNT(*) AS num_updaters FROM
        (SELECT DISTINCT ${f(join_cols[1], "devs")}, devs.issue_id, devs.updater FROM
            (SELECT ${f(join_cols[1], "issue")}, ${t("issue")}.issue_id, ${t("issue")}.updated_by AS updater
            FROM gros.${t("issue")}
            UNION ALL
            SELECT ${f(join_cols, "issue")}, comment.author AS updater
            FROM gros.comment
			JOIN gros.${t("issue")} ON comment.issue_id = ${t("issue")}.issue_id
            ) AS devs
        ) AS updaters,
        (SELECT ${t("issue")}.issue_id, MAX(${t("issue")}.changelog_id) AS changelog_id
        FROM gros.${t("issue")} GROUP BY ${t("issue")}.issue_id) AS maxdata,
        gros.${t("issue")}
    WHERE updaters.issue_id = maxdata.issue_id
    AND maxdata.issue_id = issue.issue_id AND maxdata.changelog_id = issue.changelog_id
    AND ${t("issue")}.sprint_id <> 0
    GROUP BY ${f(join_cols, "issue")}, ${t("issue")}.issue_id) AS issue_updaters
GROUP BY ${f(join_cols, "issue_updaters")}
