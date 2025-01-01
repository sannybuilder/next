import { Component, effect, model, signal } from '@angular/core';
import { Router, RouterModule } from '@angular/router';
import { CommonModule } from '@angular/common';
import { EditorComponent } from '../editor/editor.component';
import { UiService } from '../../state/ui.service';
import { FormsModule } from '@angular/forms';
import { UserService } from '../../state/user.service';
import { CodeShareService } from '../../state/code-share.service';
import { EditorService } from '../../state/editor.service';
import { catchError } from 'rxjs';

@Component({
  selector: 'app-content',
  imports: [RouterModule, CommonModule, EditorComponent, FormsModule],
  templateUrl: './content.component.html',
  styleUrl: './content.component.scss',
})
export class ContentComponent {
  title = model(this._ui.title());
  modified = this._ui.modified;
  isAuthenticated = this._user.isAuthenticated;
  saveStatus = signal(0);
  error = signal('');
  content = this._editor.source;

  constructor(
    private _ui: UiService,
    private _editor: EditorService,
    private _user: UserService,
    private _codeShare: CodeShareService,
    private _router: Router
  ) {
    effect(() => {
      const error = this.error();
      if (error) {
        this.saveStatus.set(0);
        setTimeout(() => {
          this.error.set('');
        }, 10000);
      }
    });
  }

  login() {
    this._user.login();
  }

  save() {
    const source = this._editor.source();
    const title = this.title();
    this.saveStatus.set(1);
    if (this.modified()) {
      const id = this._ui.id();
      this._codeShare
        .update(id, source, title)
        .pipe(
          catchError((e) => {
            this.error.set(e.statusText || 'Unknown error');
            return [];
          })
        )
        .subscribe((res) => {
          this._ui.modified.set(res.modified);
          this.saveStatus.set(0);
        });
    } else {
      this._codeShare
        .create(source, title)
        .pipe(
          catchError((e) => {
            this.error.set(e.statusText || 'Unknown error');
            return [];
          })
        )
        .subscribe((res) => {
          const id = res.id;
          this._ui.modified.set(res.modified);

          const login = this._user.user()?.login;
          this._editor.initialSource.set([source]);
          this._router.navigate([`/code/${login}/${id}`]);
        });
    }
  }
}
