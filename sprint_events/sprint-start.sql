SELECT ${s(project_name)} AS project_name, ${f(join_cols, "sprint", mask=2, alias=F)} AS sprint_id, ${s(sprint_name)} AS sprint_name, ${t("sprint")}.start_date AS date, ${s(sprint_close)} AS end_date, ${s(sprint_board)} AS board_id
FROM gros.${t("sprint")}
JOIN gros.${t("project")}
ON ${j(join_cols, "sprint", "project", mask=1)}
