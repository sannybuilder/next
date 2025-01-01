import { ActivatedRouteSnapshot, Router, Routes } from '@angular/router';
import { GuideComponent } from './components/guide/guide.component';
import { LayoutComponent } from './components/layout/layout.component';
import { ContentComponent } from './components/content/content.component';
import pages from './components/guide/content';
import { inject } from '@angular/core';

import { EditorService } from './state/editor.service';
import { _404Component } from './components/404/404.component';
import { UiService } from './state/ui.service';
import { UserService } from './state/user.service';
import { CodeShareService } from './state/code-share.service';
import { catchError, tap } from 'rxjs/operators';

const leaveGuard = () => {
  const editor = inject(EditorService);
  if (editor.isDirty()) {
    return confirm('You have unsaved changes. Do you want to leave?');
  }
  return true;
};

export const routes: Routes = [
  {
    path: '',
    component: LayoutComponent,
    children: [
      {
        path: '',
        redirectTo: 'guide',
        pathMatch: 'full',
      },
      {
        path: 'auth',
        resolve: [
          () => {
            const fragment = new URLSearchParams(window.location.hash.slice(1));
            const token = fragment.get('access_token');
            const userService = inject(UserService);
            const router = inject(Router);
            const deepLink = userService.getDeeplink();

            if (token) {
              userService.handleAuth(token);
            }

            if (deepLink) {
              userService.clearDeeplink();
              return router.navigateByUrl(deepLink);
            }
            return router.navigate(['/']);
          },
        ],
        children: [],
      },
      {
        path: 'guide',
        children: [
          {
            path: '',
            redirectTo: pages[0].slug,
            pathMatch: 'full',
          },
          {
            path: ':page',
            canActivate: [
              (route: ActivatedRouteSnapshot) => {
                const slug = route.params['page'];

                for (let pageId = 0; pageId < pages.length; pageId += 1) {
                  if (pages[pageId].slug === slug) {
                    inject(EditorService).setInitialSource(pages[pageId].code);
                    inject(UiService).title.set(pages[pageId].title);
                    inject(UiService).currentPage.set(pageId);
                    return true;
                  }
                }
                return inject(Router).navigate(['/guide']);
              },
            ],
            runGuardsAndResolvers: 'always',
            canDeactivate: [leaveGuard],
            component: GuideComponent,
          },
        ],
      },
      {
        path: 'code',
        children: [
          {
            path: '',
            pathMatch: 'full',
            component: ContentComponent,
            resolve: [
              () => {
                inject(EditorService).setInitialSource('');
                inject(UiService).title.set('Untitled');
                inject(UiService).modified.set(0);
              },
            ],
            runGuardsAndResolvers: 'always',
            canDeactivate: [leaveGuard],
          },
          {
            path: ':login/:id',
            component: ContentComponent,
            canActivate: [
              (route: ActivatedRouteSnapshot) => {
                const login = route.params['login'];
                const id = route.params['id'];
                const codeShare = inject(CodeShareService);
                const ui = inject(UiService);
                const editor = inject(EditorService);
                const router = inject(Router);

                return codeShare.load(login, id).pipe(
                  tap((res) => {
                    editor.setInitialSource(res.content);
                    ui.title.set(res.title);
                    ui.modified.set(res.modified);
                    ui.id.set(id);
                  }),
                  catchError(() => {
                    return router.navigate(['/404']);
                  })
                );
              },
            ],
            canDeactivate: [leaveGuard],
          },
        ],
      },

      {
        path: '**',
        component: _404Component,
        // redirectTo: '/',
      },
    ],
  },
];
