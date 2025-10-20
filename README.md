# 📄 README: Entrega N° 1 - Laboratorio de Programación (R)

Este repositorio contiene la primera entrega del trabajo práctico de R para la materia "Laboratorio para análisis de datos económicos y financieros". El proyecto se enfoca en la aplicación de herramientas de **manejo de datos (`tidyverse`)**, **análisis econométrico** y **simulación** en R.

---

## 👨‍🎓 Información del Proyecto

| Alumnos: | Noam Sade, Beltrán Saravia, Anselmo Torres, Lucca Vignoli |
| :--- | :--- |
| Fecha de Entrega: | Lunes 20 de octubre |

---

## 📦 Contenido del Repositorio

El repositorio incluye el código utilizado para los 3 ejercicios y el informe en PDF.

| Archivo | Tipo | Descripción |
| :--- | :--- | :--- |
| **`Informe_LAB_Entrega_1.pdf`** | **Informe Final (PDF)** | Documento que presenta el análisis, resultados, gráficos de cada ejercicio. |
| **`TP1_Entrega.R`** | **Código Fuente (R)** | Script de R **comentado** con todos los comandos utilizados para replicar el análisis de datos, econometría y simulación. |

---

## 🎯 Estructura y Ejercicios Desarrollados

El trabajo cubre los ejercicios de **Análisis de Datos**, **Análisis Econométrico** y el **Ejercicio de Simulación 1 (Cobb-Douglas)**.

### 1. Ejercicio de Análisis de Datos

Análisis de datos macroeconómicos en LATAM para responder tres preguntas utilizando `tidyverse`.
* **Curva de Phillips:** Exploración de la relación entre inflación y desempleo, analizando la correlación por país, nivel de ingreso y año.
* **Unidades de Medida:** Comparación del ranking de PIB per cápita bajo tres medidas diferentes (USD corrientes, USD constantes, y USD PPP constantes).
* **Ingreso y Distribución:** Relación entre PIB per cápita e Índice de Gini (desigualdad), analizando la Curva de Kuznets para Uruguay.

### 2. Ejercicio de Análisis Econométrico (`gapminder`)

* **Ingreso por Persona:** Modelado de la evolución del `income_per_person` en Argentina mediante regresiones lineales y polinómicas (grado 2 y 10) para evaluar el **overfitting**.
* **Esperanza de vida y género:** Análisis de la relación entre la `life_expectancy` general y femenina, y el impacto de variables como `income_per_person` y medidas de fertilidad.

### 3. Ejercicio de Simulación (Cobb-Douglas)

Simulación de la demanda de dos bienes bajo preferencias Cobb-Douglas.
* **Shock de Precios:** Se compara la distribución de la demanda por dos bienes distintos ante shocks en precios.
* **Heterogeneidad:** Se introduce dispersión en las preferencias para analizar cómo afecta la sensibilidad del consumo al shock de precios.
---
