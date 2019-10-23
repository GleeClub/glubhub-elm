module Page.Admin exposing (pageList)

import Html exposing (Html)
import Maybe.Extra
import Models.Event exposing (FullEvent)
import Page.Events exposing (Msg)



-- <template>
-- 	<div id="events">
-- 		<section class="section">
-- 			<div class="container">
-- 				<div class="columns">
-- 					<div class="column is-narrow">
-- 						<aside class="menu">
-- 							<div class="box">
-- 								<ul class="menu-list">
-- 									<li><router-link to="/officer/create-event">Create Event</router-link></li>
-- 									<li><router-link to="/officer/gig-requests">Gig Requests</router-link></li>
-- 									<li><router-link to="/officer/make-announcement">Make Announcement</router-link></li>
-- 									<li><router-link to="/officer/absence-requests">Absence Requests</router-link></li>
-- 									<li><router-link to="/officer/edit-semester">Edit Semester</router-link></li>
-- 									<li><router-link to="/officer/officers">Officer Positions</router-link></li>
-- 									<li><router-link to="/officer/permissions">Site Permissions</router-link></li>
-- 									<li><router-link to="/officer/uniforms">Uniforms</router-link></li>
-- 									<li><router-link to="/officer/dues">Dues</router-link></li>
-- 									<li><router-link to="/officer/document-links">Document Links</router-link></li>
-- 								</ul>
-- 							</div>
-- 						</aside>
-- 					</div>
-- 					<div class="column">
-- 						<div class="box">
-- 							<component v-if="page" :is="pageComponent"></component>
-- 							<div v-else>Select a menu item</div>
-- 						</div>
-- 					</div>
-- 				</div>
-- 			</div>
-- 		</section>
-- 	</div>
-- </template>


pageList : Html Msg
pageList =
    let
        pageLink route name =
            li [ Route.href route ] [ text name ]

        links =
            [ Route.Admin Just
            ]
    in
    ul [ class "menu-list" ]
        []



-- <script>
-- import common from "@/common"
-- import createEvent from "@/components/officer/create-event"
-- import gigRequests from "@/components/officer/gig-requests"
-- import makeAnnouncement from "@/components/officer/make-announcement"
-- import absenceRequests from "@/components/officer/absence-requests"
-- import editSemester from "@/components/officer/edit-semester"
-- import officers from "@/components/officer/officers"
-- import permissions from "@/components/officer/permissions"
-- import uniforms from "@/components/officer/uniforms"
-- import dues from "@/components/officer/dues"
-- import documentLinks from "@/components/officer/document-links"
-- export default {
-- 	name: "events",
-- 	props: ["page"],
-- 	components: {
-- 		createEvent,
-- 		gigRequests,
-- 		makeAnnouncement,
-- 		absenceRequests,
-- 		editSemester,
-- 		officers,
-- 		permissions,
-- 		uniforms,
-- 		dues,
-- 		documentLinks
-- 	},
-- 	data() {
-- 		return {
-- 			common: common
-- 		}
-- 	},
-- 	computed: {
-- 		pageComponent() {
-- 			return common.kebabToCamel(this.page)
-- 		}
-- 	}
-- }
-- </script>
