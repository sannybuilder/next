import { HttpClient, HttpHeaders } from '@angular/common/http';
import { computed, Inject, Injectable, signal } from '@angular/core';
import { resource } from '@angular/core';
import { firstValueFrom } from 'rxjs';
import CONFIG, { Config } from './config';
import { Router } from '@angular/router';
import { CookieService } from 'ngx-cookie-service';

const FAKE_TOKEN = 'fake_token';
interface UserResponse {
  login: string;
  avatar_url: string;
  html_url: string;
}

@Injectable({
  providedIn: 'root',
})
export class UserService {
  private readonly sessionKey = 'sbl.oauth.access_token';
  private readonly deepLinkPage = 'sbl.deeplink';
  private readonly clientId = 'c07f7913dd4515732ac7';

  token = signal(this._cookie.get(this.sessionKey) ?? '');

  userResource = resource({
    request: () => ({ token: this.token() }),
    loader: ({ request }) => this.getUser(request.token),
  });

  user = computed(() => {
    const user = this.userResource.value();
    return user;
  });

  isAuthenticated = computed(() => {
    const token = this.token();
    return !!token;
  });

  constructor(
    private _http: HttpClient,
    private _router: Router,
    private _cookie: CookieService,
    @Inject(CONFIG) private _config: Config
  ) {}

  login() {
    this.setDeepLink(this._router.url);

    if (!this._config.production) {
      window.location.href = `/auth#access_token=${FAKE_TOKEN}`;
      return;
    }
    const params = {
      state: generateAlphanumeric(),
      client_id: this.clientId,
      scope: 'public_repo',
      redirect_uri: window.location.origin + '/oauth2.php',
    };
    window.location.href = serializeUrlAndParams(
      'https://github.com/login/oauth/authorize',
      params
    );
  }

  getUser(token: string): Promise<UserResponse> {
    if (!token || token == FAKE_TOKEN) {
      return Promise.resolve({
        login: 'guest',
        html_url: '/',
        avatar_url: 'https://avatars.githubusercontent.com/u/16928085?s=16',
      });
    }
    const headers = new HttpHeaders({
      'Content-Type': 'application/json',
      Authorization: `Bearer ${token}`,
    });

    return firstValueFrom(
      this._http.get<UserResponse>('https://api.github.com/user', {
        headers,
      })
    );
  }

  handleAuth(token: string) {
    this.token.set(token);
    this._cookie.set(this.sessionKey, token, 14, '/');
  }

  getDeeplink() {
    return localStorage.getItem(this.deepLinkPage);
  }

  setDeepLink(value: string) {
    localStorage.setItem(this.deepLinkPage, value);
  }

  clearDeeplink() {
    localStorage.removeItem(this.deepLinkPage);
  }

  logout() {
    this.token.set('');
    this._cookie.delete(this.sessionKey, '/');
  }
}

function serializeUrlAndParams(url: string, params: object) {
  const paramsSerialized = Object.entries(params)
    .filter(([_, val]) => val !== undefined)
    .map((v) => v.join('='))
    .join('&');

  return [url, paramsSerialized].filter(Boolean).join('?');
}

function generateAlphanumeric(length: number = 14): string {
  const crypto = window.crypto;
  const array = new Uint8Array(length);
  const charset =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';

  crypto.getRandomValues(array);

  return Array.from(array)
    .map((x) => charset[x % charset.length])
    .join('');
}
