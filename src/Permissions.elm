module Permissions exposing
    ( createEvent
    , deleteUser
    , editCarpool
    , editLinks
    , editMinutes
    , editOfficers
    , editPermissions
    , editRepertoire
    , editSemester
    , editTransaction
    , editUniforms
    , processAbsenceRequests
    , processGigRequests
    , switchUser
    , viewCompleteMinutes
    , viewEventPrivateDetails
    )


editRepertoire : String
editRepertoire =
    "edit-repertoire"


viewCompleteMinutes : String
viewCompleteMinutes =
    "view-complete-minutes"


editMinutes : String
editMinutes =
    "edit-minutes"


switchUser : String
switchUser =
    "switch-user"


deleteUser : String
deleteUser =
    "delete-user"


editLinks : String
editLinks =
    "edit-links"


viewEventPrivateDetails : String
viewEventPrivateDetails =
    "view-event-private-details"


createEvent : String
createEvent =
    "create-event"


processGigRequests : String
processGigRequests =
    "process-gig-requests"


processAbsenceRequests : String
processAbsenceRequests =
    "process-absence-requests"


editSemester : String
editSemester =
    "edit-semester"


editTransaction : String
editTransaction =
    "edit-transaction"


editOfficers : String
editOfficers =
    "edit-officers"


editPermissions : String
editPermissions =
    "edit-permissions"


editUniforms : String
editUniforms =
    "edit-uniforms"


editCarpool : String
editCarpool =
    "edit-carpool"
