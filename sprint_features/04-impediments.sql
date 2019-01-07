SELECT ${f(join_cols, "issue_impediment")},
	SUM(issue_impediment.impediment) AS num_impediments
FROM (
	SELECT ${f(join_cols, "issue")}, ${t("issue")}.issue_id,
		MAX(${t("issue")}.impediment) AS impediment
	FROM gros.${t("issue")}
    WHERE ${t("issue")}.sprint_id <> 0
    GROUP BY ${f(join_cols, "issue")}, ${t("issue")}.issue_id
) AS issue_impediment
GROUP BY ${f(join_cols, "issue_impediment")}
