
--Description of selected indicators(which indicates CO2 emission) with the corresponding number of countries with data
SELECT description indicator_name, COUNT Num_of_countries
FROM (
SELECT indicator_code, count(distinct country_code) count FROM country_data
WHERE indicator_code IN ('EN_CO2_ETOT_MT','EN_ATM_CO2E_LF_KT','EG_USE_COMM_FO_ZS','EN_ATM_GHGO_KT_CE') 
GROUP BY indicator_code
) cc
INNER JOIN
indicator ind ON cc.indicator_code=ind.code
ORDER BY count DESC;

--Countries that do not have any data for CO2 indicators.
select name country_name FROM country EXCEPT
SELECT name country_name FROM country WHERE code IN (
SELECT country_code FROM country_data cd
WHERE indicator_code IN ('EN_CO2_ETOT_MT','EN_ATM_CO2E_LF_KT','EG_USE_COMM_FO_ZS','EN_ATM_GHGO_KT_CE') GROUP BY country_code
HAVING COUNT(DISTINCT indicator_code)=4)
ORDER BY country_name;

--Countries for which complete data exists for all indicators.
SELECT name country_name FROM country WHERE code IN (
SELECT country_code FROM
country_data
where indicator_code IN ('EN_CO2_ETOT_MT','EN_ATM_CO2E_LF_KT','EG_USE_COMM_FO_ZS','EN_ATM_GHGO_KT_CE') GROUP BY country_code
HAVING COUNT(DISTINCT indicator_code)=4)
ORDER BY country;

--Top 10 countries in terms of the most recent value of one of the selected indicators 
Select name country,value recent_value FROM(
SELECT country_code,date, value from country_data CD
where date=(select max(date) from country_data
where country_code=CD.country_code AND indicator_code=CD.indicator_code
AND indicator_code='EN_ATM_CO2E_LF_KT' ) )recent inner join country CC on recent.country_code=CC.code
ORDER BY value desc
LIMIT 10;

--Continents with the corresponding most recent average value (computed across countries with data) of one of the indicators.
SELECT con.name,avg(value) from
(SELECT country_code,date, value from country_data CD
where date=(select max(date) from country_data
where country_code=CD.country_code AND indicator_code=CD.indicator_code) AND value is not null AND value !=0
AND indicator_code='EN_ATM_CO2E_LF_KT') recent_val inner join continent_country cc
on cc.country_code=recent_val.country_code inner join continent con
on cc.continent_code=con.code
group by con.name ORDER BY NAME;

--Yearly Pivoted Graph
SELECT date,
avg(CASE WHEN indicator_code = 'EN_CO2_ETOT_MT' THEN value END) as EN_CO2_ETOT_MT, avg(CASE WHEN indicator_code = 'EN_ATM_CO2E_LF_KT' THEN value END) as EN_ATM_CO2E_LF_KT, avg(CASE WHEN indicator_code = 'EG_USE_COMM_FO_ZS' THEN value END) as EG_USE_COMM_FO_ZS, avg(CASE WHEN indicator_code = 'EN_ATM_GHGO_KT_CE' THEN value END) as EN_ATM_GHGO_KT_CE FROM
country_data
where country_code = 'USA'
group by date
order by date;

--The average % growth of emission for each country for all years 2010 and later for each indicator
SELECT name,round(100*AVG(GROWTH.pct_growth*100))/100 avg_pct_growth FROM(
SELECT *,CD1.value/CD0.value-1.0 pct_growth, CD1.country_code cc FROM (SELECT *,
(SELECT MAX(CD.date)
FROM country_data CD
WHERE CD.country_code=country_data.country_code AND
CD.indicator_code=country_data.indicator_code AND CD.date<country_data.date ) prev_date
FROM country_data) CD1
INNER JOIN country_data CD0 ON
CD0.country_code=CD1.country_code AND CD0.indicator_code=CD1.indicator_code AND CD0.date=CD1.prev_date
WHERE CD1.indicator_code='EN_ATM_CO2E_LF_KT'
AND extract(year from CD1.date)>2009
AND CD0.value!=0)GROWTH INNER JOIN country ON code=GROWTH.cc
GROUP BY name
ORDER BY avg_pct_growth DESC;

--countries with the highest average CO2 emission for the last 10 years of data present.
SELECT name, avg(emission) average FROM( SELECT country_code,
CASE WHEN indicator_code='EN_ATM_CO2E_LF_KT' THEN value
WHEN indicator_code='EN_CO2_ETOT_MT' THEN value*1000 END emission FROM country_data
WHERE indicator_code IN ('EN_CO2_ETOT_MT','EN_ATM_CO2E_LF_KT') and date BETWEEN '2004-12-31' AND '2014-12-31'
) emi inner join country cc on emi.country_code=cc.code
GROUP BY name
ORDER BY average DESC;

--continents that have the highest average CO2 emission rates in the past 10 years of data
SELECT con.name continent,AVG(emission) average from (
SELECT country_code country,
CASE WHEN indicator_code='EN_ATM_CO2E_LF_KT' THEN value
WHEN indicator_code='EN_CO2_ETOT_MT' THEN value*1000 END emission
FROM country_data
WHERE indicator_code IN ('EN_CO2_ETOT_MT','EN_ATM_CO2E_LF_KT') and date BETWEEN '2004- 12-31' AND '2014-12-31'
) emi
inner join continent_country cc
on cc.country_code=emi.country inner join continent con
on cc.continent_code=con.code GROUP BY con.name
order by average desc;


