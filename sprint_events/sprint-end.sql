SELECT ${s(project_name)} AS project_name, ${f(join_cols, "sprint", mask=2, alias=F)} AS sprint_id, ${t("sprint")}."name" AS sprint_name, ${s(sprint_close)} AS date
FROM gros.${t("sprint")}
JOIN gros.${t("project")}
ON ${j(join_cols, "sprint", "project", mask=1)}
