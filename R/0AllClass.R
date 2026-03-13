#' Login credentials
#'
#' @description
#' A class containing the token to communicate with the API of either the IQB Studio Lite or the IQB Testcenter. Objects of this class will be created by the function [login_studio()] or [login_testcenter()].
#'
#' @slot base_url Character. Base URL of the instance.
#' @slot base_req Function. Base [httr2] request (will be handled internally).
#' @slot ws_list Named list. Returns a list of the labels and ids of the workspaces the user has access to.
#' @slot app_version Character. The version of the instance.
#'
#' @name Login-class
#' @rdname Login-class
#'
#' @export
setClass("Login",
         slots = c(
           base_url = "character",
           base_req = "function",
           ws_list = "list",
           app_version = "character"
         ))

#' Login credentials for IQB Studio
#'
#' @description
#' A class extending the Login class with additional information for the IQB Studio. It can be created by the function [login_studio()].
#'
#' @slot base_url Character. Base URL of the IQB Studio installation.
#' @slot base_req Function. Base [httr2] request (will be handled internally).
#' @slot ws_list Named list. Returns a list of the labels and ids of the workspaces the user has access to.
#' @slot wsg_list Named list. Returns a list of the workspace groups the user has access to.
#' @slot user_id Numeric. ID of the user.
#' @slot user_key Character. Short name of the user (login name).
#' @slot user_label Character. Long name of the user.
#' @slot app_version Character. The version of the IQB Studio installation.
#'
#' @name LoginStudio-class
#' @rdname LoginStudio-class
#'
#' @export
setClass("LoginStudio",
         contains = "Login",
         slots = c(
           wsg_list = "list",
           user_id = "numeric",
           user_key = "character",
           user_label = "character"
           ))

#' Login credentials for IQB Testcenter
#'
#' @description
#' A class extending the Login class with additional information for the IQB Testcenter It can be created by the function [login_testcenter()].
#'
#' @slot base_url Character. Base URL of the IQB Testcenter installation.
#' @slot base_req Function. Base [httr2] request (will be handled internally).
#' @slot ws_list Named list. Returns a list of the labels and ids of the workspaces the user has access to.
#' @slot app_version Character. The version of the IQB Studio installation.
#'
#' @name LoginTestcenter-class
#' @rdname LoginTestcenter-class
#'
#' @export
setClass("LoginTestcenter",
         contains = "Login")

#' Workspace access
#'
#' @slot ws_id IDs of the workspaces. The workspaces IDs can also be found in the workspace URL.
#' @slot ws_label Labels of the workspaces.
#'
#' @description
#' A class containing the token to communicate with the Testcenter API. Objects of this class will be created by the function [access_workspace()] after entering a valid [Login-class] object.
#'
#' @name Workspace-class
#' @rdname Workspace-class
#'
#' @export
setClass("Workspace",
         slots = c(
           ws_id = "numeric",
           ws_label = "character"
         ))

#' Workspace access for IQB Studio
#'
#' @description
#' A class extending the Login class with additional information for the IQB Studio. It can be created by the function [login_studio()].
#'
#' @slot ws_id ID of the workspace. The workspace ID can also be found in the workspace URL.
#' @slot ws_label Label of the workspace.
#' @slot login [LoginStudio-class]. Login information for the IQB Studio.
#' @slot wsg_id ID of the workspace group the current workspace belongs to. See URL to identify the workspace group id.
#' @slot wsg_label Label of the workspace group the current workspace belongs to.
#'
#' @name WorkspaceStudio-class
#' @rdname WorkspaceStudio-class
#'
#' @export
setClass("WorkspaceStudio",
         contains = c("Workspace"),
         slots = c(
           login = "LoginStudio",
           wsg_id = "numeric",
           wsg_label = "character"
         ))

#' Workspace access for IQB Testcenter
#'
#' @description
#' A class extending the Login class with additional information for the IQB Studio. It can be created by the function [login_studio()].
#'
#' @slot login [LoginTestcenter-class]. Login information for the IQB Testcenter.
#' @slot wsg_id ID of the workspace group the current workspace belongs to. See URL to identify the workspace group id.
#' @slot wsg_label Label of the workspace group the current workspace belongs to.
#'
#' @name WorkspaceTestcenter-class
#' @rdname WorkspaceTestcenter-class
#'
#' @export
setClass("WorkspaceTestcenter",
         contains = c("Workspace"),
         slots = c(
           login = "LoginTestcenter"
         ))
