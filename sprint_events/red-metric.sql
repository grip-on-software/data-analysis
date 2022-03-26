SELECT DISTINCT ${s(project_name)} AS project_name, ${f(join_cols, "sprint", mask=2, alias=F)} AS sprint_id, ${s(sprint_name)} AS sprint_name, since_date AS date
FROM gros.${t("metric_value")}
JOIN gros.${t("project")}
ON ${j(join_cols, "metric_value", "project", mask=1)}
LEFT OUTER JOIN gros.${t("sprint")}
ON ${j(join_cols, "metric_value", "sprint")}
WHERE ${t("metric_value")}.since_date BETWEEN ${t("sprint")}.start_date AND ${s(sprint_close)}
AND category = 'red' AND EXTRACT(day FROM date - since_date) > 7
