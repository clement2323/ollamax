#' Vérifier la Disponibilité d'un Modèle via une API
#'
#' Cette fonction vérifie si un modèle spécifique est disponible en interrogeant une API donnée. Elle envoie une requête GET à l'URL de base spécifiée, récupère la liste des modèles disponibles, et détermine si le modèle souhaité est présent dans cette liste.
#'
#' @param model_name \code{character} : Le nom du modèle à vérifier.
#' @param base_url \code{character} : L'URL de base de l'API à interroger. Par défaut, \code{"https://ollama-clem.lab.sspcloud.fr"}.
#'
#' @return \code{logical} : \code{TRUE} si le modèle est disponible, \code{FALSE} sinon.
#'
#' @details
#' La fonction effectue les opérations suivantes :
#' \itemize{
#'   \item Envoie une requête GET à \code{base_url}/api/tags pour récupérer les informations des modèles disponibles.
#'   \item Vérifie si la requête a réussi (code de statut HTTP 200).
#'   \item Parse le contenu de la réponse pour extraire les noms des modèles disponibles, en retirant le suffixe \code{:latest} si présent.
#'   \item Vérifie si \code{model_name} est présent dans la liste des noms de modèles extraits.
#' }
#' 
#' Si la requête échoue ou si le modèle n'est pas trouvé, la fonction retourne \code{FALSE}.
#'
#' @import httr
#'
#' @examples
#' \dontrun{
#' # Vérifier la disponibilité d'un modèle nommé "gpt-4"
#' disponible <- check_model_availability("gpt-4")
#' if (disponible) {
#'   message("Le modèle 'gpt-4' est disponible.")
#' } else {
#'   message("Le modèle 'gpt-4' n'est pas disponible.")
#' }
#'
#' # Vérifier la disponibilité d'un modèle avec une URL de base personnalisée
#' disponible <- check_model_availability("gpt-4", base_url = "https://custom-api.example.com")
#' }
#'
#' @export
check_model_availability <- function(model_name, base_url = "https://ollama-clem.lab.sspcloud.fr") {

    if (!is.character(model_name) || length(model_name) != 1) {
        stop("Le paramètre 'model_name' doit être une chaîne de caractères unique.")
    }

    if (!is.character(base_url) || length(base_url) != 1) {
        stop("Le paramètre 'base_url' doit être une chaîne de caractères unique.")
    }


    models_response <- httr::GET(paste0(base_url, "/api/tags"))

    if (httr::status_code(models_response) == 200) {
        models <- httr::content(models_response, "parsed")
        
        # Extraire les noms des modèles et retirer le suffixe ":latest" s'il est présent
        model_names <- sapply(models[[1]], function(obj) sub(":latest$", "", obj$name))
        
        return(model_name %in% model_names)
    }

    return(FALSE)
}
